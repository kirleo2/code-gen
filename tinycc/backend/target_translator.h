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
#include "backend/instructions.hpp"
#include <map>
#include <vector>
#include "optimizer/il.h"
#include <string>
#include <cmath>
#include <type_traits>

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
  std::vector<std::pair<Register, llvm::BasicBlock*>> needToRelocate;

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
    emitInstruction(CALL, Operand(NOREG, MAIN));
    emitInstruction(HALT);
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
      emitInstruction(NOP);
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
      translatedCode[inst.first].setOp1(Operand(NOREG, (int64_t)bbToAddr[inst.second]));
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
    emitInstruction(PUSH, BP);
    emitInstruction(MOV, BP, SP);
    emitInstruction(SUB, SP, Operand(NOREG, (int64_t)stackSize));
  }

  void insertEpilogue() {
    emitInstruction(MOV, SP, BP);
    emitInstruction(POP, BP);
    emitInstruction(RET);
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
    assert(offset > 0);

    emitInstruction(LEA, getOperand(&gepInst), getOperand(ptr).setOffset((int64_t)offset).setMemoryAccess());
  }

  void translateCmpInst(llvm::CmpInst & cmpInst) {
    std::string inst;
    if (cmpInst.isIntPredicate()) {
      inst = CMP;
    } else {
      assert(cmpInst.isFPPredicate());
      inst = FCMP;
    }
    emitInstruction(inst, getOperand(cmpInst.getOperand(0)), getOperand(cmpInst.getOperand(1)));
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
      emitInstruction(MOV, R0, getOperand(retVal));
    }
    needToRelocate.emplace_back(instructionCounter, EPILOGUE);
    emitInstruction(JMP);
  }

  void translateAlloca(llvm::AllocaInst & alloca) {
    assert(allocaOffsets.count(&alloca));
    int64_t offset = -(int64_t)allocaOffsets[&alloca];
    emitInstruction(MOV, getOperand(&alloca), Operand(BP, offset).setMemoryAccess());
  }

  // Map a binary operator to corresponding t86 instruction
  void translateBinaryOperator(llvm::BinaryOperator &binOp) {
    llvm::Value *lhs = binOp.getOperand(0);
    llvm::Value *rhs = binOp.getOperand(1);
    std::string inst;
    switch (binOp.getOpcode()) {
      case llvm::Instruction::Add:
        inst = ADD;
        break;
      case llvm::Instruction::FAdd:
        inst = FADD;
        break;
      case llvm::Instruction::Sub:
        inst = SUB;
        break;
      case llvm::Instruction::FSub:
        inst = FSUB;
        break;
      case llvm::Instruction::Mul:
        inst = IMUL;
        break;
      case llvm::Instruction::FMul:
        inst = FMUL;
        break;
      case llvm::Instruction::SDiv:
        inst = IDIV;
        break;
      case llvm::Instruction::FDiv:
        inst = FDIV;
        break;
      case llvm::Instruction::SRem:
        // TODO: % operation
        UNREACHABLE;
        break;
      case llvm::Instruction::Shl:
        inst = LSH;
        break;
      case llvm::Instruction::AShr:
        // shift with saving of the sign
        // TODO: Does t86 support it?
        inst = RSH;
        break;
      case llvm::Instruction::And:
        inst = AND;
        break;
      case llvm::Instruction::Or:
        inst = OR;
        break;
      case llvm::Instruction::Xor:
        inst = XOR;
        break;
      default:
        llvm::errs() << "Unsupported binary operation: " << binOp << "\n";
        UNREACHABLE;
        break;
    }
    emitInstruction(inst, getOperand(lhs), getOperand(rhs));
    // Because target works with 2 registers, but LLVM requires one more to store result,
    // temporary workaround is to put result into new register.
    emitInstruction(MOV, getOperand(&binOp), getOperand(lhs));
  }

  // When translating store from function argument, we want to treat it as store from the stack, because
  // arguments are stored on the stack (calling convention)
  void translateStoreInst(llvm::StoreInst &store) {
    llvm::Value *value = store.getValueOperand();
    llvm::Value *ptr = store.getPointerOperand();
    std::stringstream inst;
    Operand op1 (getOperand(ptr));
    op1.setMemoryAccess();
    if (llvm::Argument* argument = llvm::dyn_cast<llvm::Argument>(value)) {
      Operand op2 = (BP, (int64_t)(argument->getParent()->arg_size() - argument->getArgNo()));
      emitInstruction(MOV, op1, op2.setMemoryAccess());
    } else if (llvm::ConstantInt *constInt = llvm::dyn_cast<llvm::ConstantInt>(value)) {
      emitInstruction(Instruction(MOV, op1, Operand(NOREG, constInt->getSExtValue())));
    } else if (llvm::LoadInst *loadInst = llvm::dyn_cast<llvm::LoadInst>(value)) {
      emitInstruction(Instruction(MOV, op1, getOperand(loadInst)));
    } else if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
      emitInstruction(MOV, op1, getOperand(allocaInst).setMemoryAccess());
    } else {
      llvm::errs() << "Unsupported store: " << store << "\n";
      UNREACHABLE;
    }
  }

  void translateLoadInst(llvm::LoadInst &load) {
    llvm::Value *ptr = load.getPointerOperand();
    emitInstruction(Instruction("MOV", Operand(getOperand(&load)), Operand(getOperand(ptr)).setMemoryAccess()));
  }

  void emitJump(const std::string & inst, llvm::BasicBlock* bb) {
    Instruction instruction(inst);
    if (bbToAddr.count(bb)) {
      instruction.setOp1(Operand(NOREG, (int64_t) bbToAddr[bb]));
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
          inst = JE;
          break;
        case llvm::CmpInst::FCMP_OGT:
          inst = JG;
          break;
        case llvm::CmpInst::FCMP_OGE:
          inst = JGE;
          break;
        case llvm::CmpInst::FCMP_OLT:
          inst = JL;
          break;
        case llvm::CmpInst::FCMP_OLE:
          inst = JLE;
          break;
        case llvm::CmpInst::FCMP_ONE:
          inst = JNE;
          break;
        case llvm::CmpInst::ICMP_EQ:
          inst = JE;
          break;
        case llvm::CmpInst::ICMP_NE:
          inst = JNE;
          break;
        case llvm::CmpInst::ICMP_SGT:
          inst = JG;
          break;
        case llvm::CmpInst::ICMP_SGE:
          inst = JGE;
          break;
        case llvm::CmpInst::ICMP_SLT:
          inst = JL;
          break;
        case llvm::CmpInst::ICMP_SLE:
          inst = JLE;
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
    inst = JMP;
    emitJump(inst, bb);
  }

  // R0 is return register
  void translateCallInst(llvm::CallInst &call) {
    // TODO: struct ABI
    // temporary: push function arguments onto the stack
    llvm::Function* function = call.getCalledFunction();

    // handle intrinsics
    if (function->getName() == "putint") {
      emitInstruction(Instruction(PUTNUM, getOperand(call.getOperand(0))));
    } else if (function->getName() == "putchar") {
      emitInstruction(Instruction(MOV, R1, getOperand(call.getOperand(0))));
      emitInstruction(Instruction(PUTCHAR, getOperand(call.getOperand(0))));
    } else if (function->getName() == "getchar") {
      emitInstruction(Instruction(GETCHAR, R0));
    } else {
      for (auto & arg : call.operands()) {
        emitInstruction(Instruction(PUSH, getOperand(arg)));
      }
      // clear args
      needToRelocate.emplace_back(instructionCounter, &function->getEntryBlock());
      emitInstruction(CALL);

      if (!function->getReturnType()->isVoidTy()) {
        emitInstruction(MOV, getOperand(&call), R0);
      }
    }

  }

  template <typename... Ts>
  void emitInstruction(Ts&&... args) {
    instructionCounter++;
    translatedCode.emplace_back(std::forward<Ts>(args)...);
  }

  // TODO: Mapping for constraint number of registers
  // Custom method for mapping LLVM values to registers in t86
  Operand getOperand(llvm::Value *v) {
    // TODO: distinguish float and int registers
   if (llvm::ConstantInt *constInt = llvm::dyn_cast<llvm::ConstantInt>(v)) {
      // e.g. ADD R2, 3 and we want to get register for immediate 3
      return Operand(NOREG, constInt->getSExtValue());
    }

    if (registers.count(v) == 0) {
      registers[v] = registers.size() + R1 + 1;
    }

    return Operand(registers[v]);  // Simple register mapping
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
