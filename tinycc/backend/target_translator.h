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
#include <math.h>

using namespace tiny;

// Macro to apply synthetic BasicBlock, represented by invalid pointer
#define EPILOGUE (llvm::BasicBlock*) (currentEpilogue)

class TargetTranslator {
  size_t instructionCounter;                      // Counter for instruction addresses
  std::vector<std::string> translatedCode;    // Container for translated instructions
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
    emitInstruction("CALL 2");
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
      translatedCode[inst.first] += std::to_string(bbToAddr[inst.second]);
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
    emitInstruction("PUSH BP");
    emitInstruction("MOV BP, SP");
    emitInstruction("SUB SP, " + std::to_string(stackSize));
  }

  void insertEpilogue() {
    emitInstruction("MOV SP, BP");
    emitInstruction("POP BP");
    emitInstruction("RET");
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

  }

  void translateCmpInst(llvm::CmpInst & cmpInst) {
    std::stringstream inst;
    if (cmpInst.isIntPredicate()) {
      inst << "CMP ";
    } else {
      assert(cmpInst.isFPPredicate());
      inst << "FCMP ";
    }
    inst << getReg(cmpInst.getOperand(0)) << ", " << getReg(cmpInst.getOperand(1));
    emitInstruction(inst);
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
    emitInstruction("JMP ");
  }

  void translateAlloca(llvm::AllocaInst & alloca) {
  }

  // Map a binary operator to corresponding t86 instruction
  void translateBinaryOperator(llvm::BinaryOperator &binOp) {
    llvm::Value *lhs = binOp.getOperand(0);
    llvm::Value *rhs = binOp.getOperand(1);

    std::stringstream inst;

    switch (binOp.getOpcode()) {
      case llvm::Instruction::Add:
        inst << "ADD " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::FAdd:
        inst << "FADD " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::Sub:
        inst << "SUB " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::FSub:
        inst << "FSUB " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::Mul:
        inst << "IMUL " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::FMul:
        inst << "FMUL " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::SDiv:
        inst << "IDIV " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::FDiv:
        inst << "FDIV " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::SRem:
        // TODO: % operation
        UNREACHABLE;
        break;
      case llvm::Instruction::Shl:
        inst << "LSH " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::AShr:
        // shift with saving of the sign
        // TODO: Does t86 support it?
        inst << "RSH " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::And:
        inst << "AND " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::Or:
        inst << "OR " << getReg(lhs) << ", " << getReg(rhs);
        break;
      case llvm::Instruction::Xor:
        inst << "XOR " << getReg(lhs) << ", " << getReg(rhs);
        break;
      default:
        llvm::errs() << "Unsupported binary operation: " << binOp << "\n";
        UNREACHABLE;
        break;
    }

    emitInstruction(inst);
    // Because target works with 2 registers, but LLVM requires one more to store result,
    // temporary workaround is to put result into new register.
    inst << "MOV " << getReg(&binOp) << ", " << getReg(lhs);
    emitInstruction(inst);
  }

  // When translating store from function argument, we want to treat it as store from the stack, because
  // arguments are stored on the stack (calling convention)
  void translateStoreInst(llvm::StoreInst &store) {
    llvm::Value *value = store.getValueOperand();
    llvm::Value *ptr = store.getPointerOperand();
    std::stringstream inst;

    if (llvm::Argument* argument = llvm::dyn_cast<llvm::Argument>(value)) {
      inst << "MOV [" << getReg(ptr) << "], [BP + " << std::to_string(argument->getParent()->arg_size() - argument->getArgNo()) << "]";
    } else if (llvm::ConstantInt *constInt = llvm::dyn_cast<llvm::ConstantInt>(value)) {
      inst << "MOV " << "[" << getReg(ptr) << "], " << constInt->getZExtValue();
    } else if (llvm::Instruction *loadInst = llvm::dyn_cast<llvm::Instruction>(value)) {
      inst << "MOV " << "[" << getReg(ptr) << "], " << getReg(loadInst);
    } else {
      llvm::errs() << "Unsupported store: " << store << "\n";
      UNREACHABLE;
    }
    emitInstruction(inst);
  }

  void translateLoadInst(llvm::LoadInst &load) {
    llvm::Value *ptr = load.getPointerOperand();
    std::stringstream inst;
    inst << "MOV "  << getReg(&load) << ", [" << getReg(ptr) << "]";
    emitInstruction(inst);
  }

  void emitJump(std::stringstream & inst, llvm::BasicBlock* bb) {
    if (bbToAddr.count(bb)) {
      inst << bbToAddr.at(bb);
    } else {
      needToRelocate.emplace_back(instructionCounter, bb);
    }
    emitInstruction(inst);
  }

  void translateBranchInst(llvm::BranchInst &branch) {
    std::stringstream inst;
    llvm::BasicBlock* bb = nullptr;
    if (branch.isConditional()) {
      llvm::CmpInst* cmp = llvm::dyn_cast<llvm::CmpInst>(branch.getCondition());
      llvm::BasicBlock* bb_true = branch.getSuccessor(0);
      // false case
      bb = branch.getSuccessor(1);
      switch (cmp->getPredicate()) {
        case llvm::CmpInst::FCMP_OEQ:
          inst << "JE ";
          break;
        case llvm::CmpInst::FCMP_OGT:
          inst << "JG ";
          break;
        case llvm::CmpInst::FCMP_OGE:
          inst << "JGE ";
          break;
        case llvm::CmpInst::FCMP_OLT:
          inst << "JL ";
          break;
        case llvm::CmpInst::FCMP_OLE:
          inst << "JLE ";
          break;
        case llvm::CmpInst::FCMP_ONE:
          inst << "JNE ";
          break;
        case llvm::CmpInst::ICMP_EQ:
          inst << "JE ";
          break;
        case llvm::CmpInst::ICMP_NE:
          inst << "JNE ";
          break;
        case llvm::CmpInst::ICMP_SGT:
          inst << "JG ";
          break;
        case llvm::CmpInst::ICMP_SGE:
          inst << "JGE ";
          break;
        case llvm::CmpInst::ICMP_SLT:
          inst << "JL ";
          break;
        case llvm::CmpInst::ICMP_SLE:
          inst << "JLE ";
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
    inst << "JMP ";
    emitJump(inst, bb);
  }

  // R0 is return register
  void translateCallInst(llvm::CallInst &call) {
    // TODO: struct ABI
    // temporary: push function arguments onto the stack
    llvm::Function* function = call.getCalledFunction();
    std::stringstream inst;
    // handle intrinsics
    if (function->getName() == "putint") {
      emitInstruction("PUTNUM " + getReg(call.getOperand(0)));
    } else if (function->getName() == "putchar") {
      inst << "MOV R1, " << getReg(call.getOperand(0));
      emitInstruction(inst);
      emitInstruction("PUTCHAR " + getReg(call.getOperand(0)));
    } else if (function->getName() == "getchar") {
      emitInstruction("GETCHAR R0");
    } else {
      for (auto & arg : call.operands()) {
        emitInstruction("PUSH " + getReg(arg));
      }
      // clear args
      std::string instruction = "CALL ";
      needToRelocate.emplace_back(instructionCounter, &function->getEntryBlock());
      emitInstruction(instruction);

      if (!function->getReturnType()->isVoidTy()) {
        emitInstruction("MOV " + getReg(&call) + ", R0");
      }
    }

  }

  // Emit a translated instruction
  void emitInstruction(std::stringstream & instruction) {
    translatedCode.push_back(std::to_string(instructionCounter++) + "\t" + instruction.str());
    instruction.str("");
  }

  void emitInstruction(const std::string & instruction) {
    translatedCode.push_back(std::to_string(instructionCounter++) + "\t" + instruction);
  }

  // TODO: Mapping for constraint number of registers
  // Custom method for mapping LLVM values to registers in t86
  std::string getReg(llvm::Value *v) {
    // TODO: distinguish float and int registers
    // fetch local variable from memory
    if (llvm::AllocaInst* allocaInst = llvm::dyn_cast<llvm::AllocaInst>(v)) {
      std::stringstream operand;
      assert(allocaOffsets.count(allocaInst));
      operand << "BP - " << allocaOffsets[allocaInst];
      return operand.str();
    } else if (llvm::ConstantInt *constInt = llvm::dyn_cast<llvm::ConstantInt>(v)) {
      return std::to_string(constInt->getSExtValue());
    }

    if (registers.count(v) == 0) {
      registers[v] = registers.size() + 1;
    }
    return "R" + std::to_string(registers[v]);  // Simple register mapping
  }

  // Emit the translated code to the specified output file
  bool emitMachineCode(const std::string &outputFile) {
    std::error_code EC;
    llvm::raw_fd_ostream file(outputFile, EC, llvm::sys::fs::OF_Text);
    if (EC) {
      llvm::errs() << "Error opening file " << outputFile << ": " << EC.message() << "\n";
      return false;
    }

    file << ".text\n";
    for (const auto &line : translatedCode) {
      file << line << "\n";
    }

    file.close();
    return true;
  }
};

#endif // TINYCC_TARGET_TRANSLATOR_H
