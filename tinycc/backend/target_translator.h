#ifndef TINYCC_TARGET_TRANSLATOR_H
#define TINYCC_TARGET_TRANSLATOR_H

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/BasicBlock.h>
#include "llvm/IR/Constants.h"
#include <map>
#include <vector>
#include "optimizer/il.h"
#include <string>
#include <cmath>
#include <type_traits>

typedef size_t Register;

class Operand {

public:
  template <typename T, typename = typename std::enable_if<std::is_integral<T>::value>::type>
  Operand (T i): op(std::to_string(i)), isMemoryAccess(false) {}
  Operand(const std::string & op): op(op), isMemoryAccess(false) {}
  Operand(const std::string & op, bool isMemoryAccess): op(op), isMemoryAccess(isMemoryAccess) {}

  Operand & setMemoryAccess() {
    isMemoryAccess = true;
    return *this;
  }

  const std::string & getOperand() const {
    if (isMemoryAccess) {
      return "[" + op + "]";
    }
    return op;
  }

  friend std::ostream& operator<<(std::ostream& os, const Operand & op) {
    if (op.isMemoryAccess) {
      os << "[" << op.op << "]";
    } else {
      os << op.op;
    }
    return os;
  }

private:
  std::string op;
  bool isMemoryAccess;
};

class Instruction {
public:
  Instruction() {}
  Instruction(const std::string & inst): instruction(inst) {}
  Instruction(const std::string & inst, const Operand & op1): instruction(inst), op1(op1) {}
  Instruction(const std::string & inst, const Operand & op1, const Operand & op2): instruction(inst), op1(op1), op2(op2) {}

  void setInstruction(const std::string & inst) {
    instruction = inst;
  }

  void setOp1(const Operand & op) {
    op1 = op;
  }

  void setOp2(const Operand & op) {
    op2 = op;
  }

  friend std::ostream& operator<<(std::ostream& os, const Instruction& inst) {
    os << inst.instruction;
    if (inst.op1.has_value()) {
      os << " " << inst.op1.value();
    }
    if (inst.op2.has_value()) {
      assert(inst.op1.has_value());
      os << ", " << inst.op2.value();
    }
    return os;
  }

private:
  std::string instruction;
  std::optional<Operand> op1;
  std::optional<Operand> op2;
};

using namespace tiny;

// Macro to apply synthetic BasicBlock, represented by invalid pointer
#define EPILOGUE (llvm::BasicBlock*) (currentEpilogue)

class TargetTranslator {
  size_t instructionCounter;                      // Counter for instruction addresses
  std::vector<Instruction> translatedCode;    // Container for translated instructions
  Program program;
  size_t currentEpilogue;
  std::unordered_map<llvm::BasicBlock*, size_t> bbToAddr;
  std::unordered_map<llvm::Value*, size_t> registers;
  std::unordered_map<llvm::AllocaInst*, size_t> allocaOffsets;
  std::unordered_map<llvm::Function*, size_t> epilogueOffsets;
  std::vector<std::pair<size_t, llvm::BasicBlock*>> needToRelocate;

public:
  TargetTranslator(Program & program) : program(program) {
    currentEpilogue = 0;
    // Initialize necessary components (e.g., registers, target settings)
    instructionCounter = 0;
  }

  // Firstly assign to each BB its address in assembly file then
  // Translate the provided LLVM IR to t86 instructions
  bool translateToTarget(const std::string &outputFile) {
    // Iterate through functions and translate each function
    emitInstruction("CALL", 2);
    emitInstruction("HALT");
    for (llvm::Function &F : *program.module) {
      if (F.isDeclaration()) continue; // Skip external functions
      size_t stackSize = calculateStackSize(F);
      // insert prolog
      insertPrologue(stackSize);
      // Process each basic block within the function
      for (llvm::BasicBlock &BB : F) {
        bbToAddr[&BB] = instructionCounter;
        for (llvm::Instruction &I : BB) {
          translateInstruction(I); // Translate each instruction
        }
      }
      assert(!bbToAddr.count(EPILOGUE));
      bbToAddr[EPILOGUE] = instructionCounter;
      // insert epilogue
      insertEpilogue();
      currentEpilogue++;

      // divide functions
      emitInstruction("NOP");
    }
    patchJumps();
    // Emit the generated machine code to a file (custom format)
    return emitMachineCode(outputFile);
  }

private:
  // We do not know exact address of each basic block during the jump generation,
  // but we can 'relocate' them after processing of all instructions,
  // when exact address of each basic block is known
  void patchJumps() {
    for (auto & inst : needToRelocate) {
      assert(bbToAddr.count(inst.second));
      translatedCode[inst.first].setOp1(bbToAddr[inst.second]);
    }
  }

  size_t calculateTypeSize(llvm::Type* type) const {
    const llvm::DataLayout & layout = program.module->getDataLayout();
    size_t size = layout.getTypeSizeInBits(type);
    return (size + 63) / 64;
  }

  size_t calculateStackSize(llvm::Function & function) {
    allocaOffsets.clear();
    size_t size = 0;
    for (llvm::BasicBlock &BB : function) {
      for (llvm::Instruction &I : BB) {
        if (llvm::AllocaInst *alloca = llvm::dyn_cast<llvm::AllocaInst>(&I)) {
          size += calculateTypeSize(alloca->getAllocatedType());
          allocaOffsets[alloca] = size;
        }
      }
    }
    return size;
  }

  void insertPrologue(size_t stackSize) {
    emitInstruction(Instruction("PUSH", Operand("BP")));
    emitInstruction(Instruction("MOV", Operand("BP"), Operand("SP")));
    emitInstruction(Instruction("SUB", Operand("SP"), Operand(stackSize)));
  }

  void insertEpilogue() {
    emitInstruction(Instruction("MOV", Operand("SP"), Operand("BP")));
    emitInstruction(Instruction("POP", Operand("BP")));
    emitInstruction(Instruction("RET"));
  }

  // Main function to handle translation of LLVM IR to t86 instructions
  void translateInstruction(llvm::Instruction &I) {
    if (llvm::BinaryOperator *binOp = llvm::dyn_cast<llvm::BinaryOperator>(&I)) {
      translateBinaryOperator(*binOp);
    } else if (llvm::StoreInst *store = llvm::dyn_cast<llvm::StoreInst>(&I)) {
      translateStoreInst(*store);
    } else if (llvm::LoadInst *load = llvm::dyn_cast<llvm::LoadInst>(&I)) {
      translateLoadInst(*load);
    } else if (llvm::BranchInst *branch = llvm::dyn_cast<llvm::BranchInst>(&I)) {
      translateBranchInst(*branch);
    } else if (llvm::CallInst *call = llvm::dyn_cast<llvm::CallInst>(&I)) {
      translateCallInst(*call);
    } else if (llvm::AllocaInst *alloca = llvm::dyn_cast<llvm::AllocaInst>(&I)) {
      translateAlloca(*alloca);
    } else if (llvm::ReturnInst* retInst = llvm::dyn_cast<llvm::ReturnInst>(&I)) {
      translateReturn(*retInst);
    } else if (llvm::CmpInst* cmpInst = llvm::dyn_cast<llvm::CmpInst>(&I)) {
      translateCmpInst(*cmpInst);
    } else if (llvm::GetElementPtrInst * gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(&I)) {
      translateGepInst(*gepInst);
    } else {
        llvm::errs() << "Unsupported instruction: " << I << "\n";
        UNREACHABLE;
    }
  }

  void translateGepInst(llvm::GetElementPtrInst & gepInst) {
    std::stringstream inst;
    llvm::Value* ptr = gepInst.getPointerOperand();
    llvm::Type* type = gepInst.getSourceElementType();
    size_t type_size = calculateTypeSize(type);
    size_t offset = 0;
    for (auto & index : gepInst.indices()) {
      assert(llvm::isa<llvm::ConstantInt>(index));
      uint64_t constIdx = llvm::dyn_cast<llvm::ConstantInt>(index)->getZExtValue();
      if (index == *gepInst.idx_begin()) {
        offset += type_size * constIdx;
      } else {
        assert(type->isStructTy());
        llvm::StructType * structType = llvm::dyn_cast<llvm::StructType>(type);
        llvm::Type* elementType = structType->getElementType(constIdx);
        for (size_t i = 0; i < constIdx; ++i) offset += calculateTypeSize(structType->getElementType(i));
        type = elementType;
      }
    }

    if (offset == 0)  {
      emitInstruction("MOV", getReg(&gepInst), getReg(ptr));
    } else {
      std::string op2 = getReg(ptr);
      op2 += " + " + std::to_string(offset);
      emitInstruction("LEA", getReg(&gepInst), Operand(op2).setMemoryAccess());
    }
  }

  void translateCmpInst(llvm::CmpInst & cmpInst) {
    std::string inst;
    if (cmpInst.isIntPredicate()) {
      inst = "CMP";
    } else {
      assert(cmpInst.isFPPredicate());
      inst = "FCMP";
    }
    emitInstruction(inst, getReg(cmpInst.getOperand(0)), getReg(cmpInst.getOperand(1)));
  }

  void translateReturn (llvm::ReturnInst & retInst) {
    // R0 will be return register
    // TODO: Struct ABI
    llvm::Value* retVal = retInst.getReturnValue();
    if (retVal->getType()->isStructTy()) {
      llvm::errs() << "Structs are not supported!" << "\n";
      UNREACHABLE;
    }
    if (!retVal->getType()->isVoidTy()) {
      emitInstruction("MOV R0, " + getReg(retVal));
    }
    needToRelocate.emplace_back(instructionCounter, EPILOGUE);
    emitInstruction("JMP");
  }

  void translateAlloca(llvm::AllocaInst & alloca) {
    assert(allocaOffsets.count(&alloca));
    Operand op1 (getReg(&alloca));
    Operand op2 ("BP - " + std::to_string(allocaOffsets[&alloca]));
    op2.setMemoryAccess();
    emitInstruction("MOV", op1, op2);
  }

  // Map a binary operator to corresponding t86 instruction
  void translateBinaryOperator(llvm::BinaryOperator &binOp) {
    llvm::Value *lhs = binOp.getOperand(0);
    llvm::Value *rhs = binOp.getOperand(1);
    Operand op1 = getReg(lhs);
    Operand op2 = getReg(rhs);
    std::string inst;
    switch (binOp.getOpcode()) {
      case llvm::Instruction::Add:
        inst = "ADD";
        break;
      case llvm::Instruction::FAdd:
        inst = "FADD";
        break;
      case llvm::Instruction::Sub:
        inst = "SUB";
        break;
      case llvm::Instruction::FSub:
        inst = "FSUB";
        break;
      case llvm::Instruction::Mul:
        inst = "IMUL";
        break;
      case llvm::Instruction::FMul:
        inst = "FMUL";
        break;
      case llvm::Instruction::SDiv:
        inst = "IDIV";
        break;
      case llvm::Instruction::FDiv:
        inst = "FDIV";
        break;
      case llvm::Instruction::SRem:
        // TODO: % operation
        UNREACHABLE;
        break;
      case llvm::Instruction::Shl:
        inst = "LSH";
        break;
      case llvm::Instruction::AShr:
        // shift with saving of the sign
        // TODO: Does t86 support it?
        inst = "RSH";
        break;
      case llvm::Instruction::And:
        inst = "AND";
        break;
      case llvm::Instruction::Or:
        inst = "OR";
        break;
      case llvm::Instruction::Xor:
        inst = "XOR";
        break;
      default:
        llvm::errs() << "Unsupported binary operation: " << binOp << "\n";
        UNREACHABLE;
        break;
    }
    emitInstruction(inst, op1, op2);
    // Because target works with 2 registers, but LLVM requires one more to store result,
    // temporary workaround is to put result into new register.
    emitInstruction("MOV", getReg(&binOp), getReg(lhs));
  }

  // When translating store from function argument, we want to treat it as store from the stack, because
  // arguments are stored on the stack (calling convention)
  void translateStoreInst(llvm::StoreInst &store) {
    llvm::Value *value = store.getValueOperand();
    llvm::Value *ptr = store.getPointerOperand();
    std::stringstream inst;
    Operand op1 (getReg(ptr));
    op1.setMemoryAccess();
    if (llvm::Argument* argument = llvm::dyn_cast<llvm::Argument>(value)) {
      Operand op2 ("BP + " + std::to_string(argument->getParent()->arg_size() - argument->getArgNo()), true);
      emitInstruction(Instruction("MOV", op1, op2));
    } else if (llvm::ConstantInt *constInt = llvm::dyn_cast<llvm::ConstantInt>(value)) {
      emitInstruction(Instruction("MOV", op1, Operand(constInt->getZExtValue())));
    } else if (llvm::LoadInst *loadInst = llvm::dyn_cast<llvm::LoadInst>(value)) {
      emitInstruction(Instruction("MOV", op1, getReg(loadInst)));
    } else if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
      emitInstruction("MOV", op1, Operand(getReg(allocaInst)).setMemoryAccess());
    } else {
      llvm::errs() << "Unsupported store: " << store << "\n";
      UNREACHABLE;
    }
  }

  void translateLoadInst(llvm::LoadInst &load) {
    llvm::Value *ptr = load.getPointerOperand();
    emitInstruction(Instruction("MOV", Operand(getReg(&load)), Operand(getReg(ptr)).setMemoryAccess()));
  }

  void emitJump(const std::string & inst, llvm::BasicBlock* bb) {
    Instruction instruction(inst);
    if (bbToAddr.count(bb)) {
      instruction.setOp1(std::to_string(bbToAddr[bb]));
    } else {
      needToRelocate.emplace_back(instructionCounter, bb);
    }
    emitInstruction(inst);
  }

  void translateBranchInst(llvm::BranchInst &branch) {
    std::string inst;
    llvm::BasicBlock* bb = nullptr;
    if (branch.isConditional()) {
      llvm::CmpInst* cmp = llvm::dyn_cast<llvm::CmpInst>(branch.getCondition());
      llvm::BasicBlock* bb_true = branch.getSuccessor(0);
      // false case
      bb = branch.getSuccessor(1);
      switch (cmp->getPredicate()) {
        case llvm::CmpInst::FCMP_OEQ:
          inst = "JE";
          break;
        case llvm::CmpInst::FCMP_OGT:
          inst = "JG";
          break;
        case llvm::CmpInst::FCMP_OGE:
          inst = "JGE";
          break;
        case llvm::CmpInst::FCMP_OLT:
          inst = "JL";
          break;
        case llvm::CmpInst::FCMP_OLE:
          inst = "JLE";
          break;
        case llvm::CmpInst::FCMP_ONE:
          inst = "JNE";
          break;
        case llvm::CmpInst::ICMP_EQ:
          inst = "JE";
          break;
        case llvm::CmpInst::ICMP_NE:
          inst = "JNE";
          break;
        case llvm::CmpInst::ICMP_SGT:
          inst = "JG";
          break;
        case llvm::CmpInst::ICMP_SGE:
          inst = "JGE";
          break;
        case llvm::CmpInst::ICMP_SLT:
          inst = "JL";
          break;
        case llvm::CmpInst::ICMP_SLE:
          inst = "JLE";
          break;
        default:
          UNREACHABLE;
          break;
      }
      emitJump(inst, bb_true);
    } else {
      bb = branch.getSuccessor(0);
    }
    // emit false case for conditional and usual jump for unconditional
    inst = "JMP";
    emitJump(inst, bb);
  }

  // R0 is return register
  void translateCallInst(llvm::CallInst &call) {
    // TODO: struct ABI
    // temporary: push function arguments onto the stack
    llvm::Function* function = call.getCalledFunction();

    // handle intrinsics
    if (function->getName() == "putint") {
      emitInstruction(Instruction("PUTNUM", getReg(call.getOperand(0))));
    } else if (function->getName() == "putchar") {
      emitInstruction(Instruction("MOV", Operand("R1"), getReg(call.getOperand(0))));
      emitInstruction(Instruction("PUTCHAR", getReg(call.getOperand(0))));
    } else if (function->getName() == "getchar") {
      emitInstruction(Instruction("GETCHAR", Operand("R0")));
    } else {
      for (auto & arg : call.operands()) {
        emitInstruction(Instruction("PUSH", getReg(arg)));
      }
      // clear args
      needToRelocate.emplace_back(instructionCounter, &function->getEntryBlock());
      emitInstruction(Instruction("CALL"));

      if (!function->getReturnType()->isVoidTy()) {
        emitInstruction(Instruction("MOV", getReg(&call), Operand("R0")));
      }
    }

  }

//  // Emit a translated instruction
//  void emitInstruction(std::stringstream & instruction) {
//    translatedCode.push_back(std::to_string(instructionCounter++) + "\t" + instruction.str());
//    instruction.str("");
//  }

  template <typename... Ts>
  void emitInstruction(Ts&&... args) {
    instructionCounter++;
    translatedCode.emplace_back(std::forward<Ts>(args)...);
  }

  void emitInstruction(const Instruction & inst) {
    instructionCounter++;
    translatedCode.push_back(inst);
  }

  // TODO: Mapping for constraint number of registers
  // Custom method for mapping LLVM values to registers in t86
  std::string getReg(llvm::Value *v) {
    // TODO: distinguish float and int registers
   if (llvm::ConstantInt *constInt = llvm::dyn_cast<llvm::ConstantInt>(v)) {
      // e.g. ADD R2, 3 and we want to get register for immediate 3
      return std::to_string(constInt->getSExtValue());
    }

    if (registers.count(v) == 0) {
      registers[v] = registers.size() + 1;
    }
    return "R" + std::to_string(registers[v]);  // Simple register mapping
  }

  // Emit the translated code to the specified output file
  bool emitMachineCode(const std::string &outputFile) {
    std::ofstream file(outputFile);
    if (!file.is_open()) {
      llvm::errs() << "Error opening file " << outputFile  << "\n";
      return false;
    }

    file << ".text\n";
    for (size_t i = 0; i < translatedCode.size(); i++) {
      file << i << "\t" << translatedCode[i] << "\n";
    }

    file.close();
    return true;
  }
};

#endif // TINYCC_TARGET_TRANSLATOR_H
