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
      insertProlog(stackSize);
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
      insertEpilog(stackSize);
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

  void insertProlog(size_t stackSize) {
    emitInstruction("PUSH BP");
    emitInstruction("MOV BP, SP");
    emitInstruction("SUB SP, " + std::to_string(stackSize));
    // TODO: load function args from stack ?
  }

  void insertEpilog(size_t stackSize) {
    emitInstruction("ADD SP, " + std::to_string(stackSize));
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
    } else {
        llvm::errs() << "Unsupported instruction: " << I << "\n";
        UNREACHABLE;
    }
  }

  void translateReturn (llvm::ReturnInst & retInst) {
    // R0 will be return register
    llvm::Value* retVal = retInst.getReturnValue();
    if (retVal != nullptr) {
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

    if (binOp.getOpcode() == llvm::Instruction::Add) {
      inst << "ADD " << getReg(lhs) << ", " << getReg(rhs);
    } else if (binOp.getOpcode() == llvm::Instruction::Sub) {
      inst << "SUB " << getReg(lhs) << ", " + getReg(rhs);
    } else {
      llvm::errs() << "Unsupported binary operation: " << binOp << "\n";
      return;
    }
    emitInstruction(inst.str());
    // Because target works with 2 registers, but LLVM requires one more to store result,
    // temporary workaround is to put result into new register.
    inst.str("");
    inst << "MOV " << getReg(&binOp) << ", " << getReg(lhs);
    emitInstruction(inst.str());
  }

  void translateStoreInst(llvm::StoreInst &store) {
    llvm::Value *value = store.getValueOperand();
    llvm::Value *ptr = store.getPointerOperand();
    std::stringstream inst;
    if (llvm::ConstantInt *constInt = llvm::dyn_cast<llvm::ConstantInt>(value)) {
      inst << "MOV " << "[" << getReg(ptr) << "], " << constInt->getZExtValue();
    } else if (llvm::Instruction *loadInst = llvm::dyn_cast<llvm::Instruction>(value)) {
      inst << "MOV " << "[" << getReg(ptr) << "], " << getReg(loadInst);
    } else {
      llvm::errs() << "Unsupported store: " << store << "\n";
      UNREACHABLE;
    }
    emitInstruction(inst.str());
  }

  void translateLoadInst(llvm::LoadInst &load) {
    llvm::Value *ptr = load.getPointerOperand();
    std::stringstream inst;
    inst << "MOV "  << getReg(&load) << ", [" << getReg(ptr) << "]";
    emitInstruction(inst.str());
  }

  void translateBranchInst(llvm::BranchInst &branch) {
    std::stringstream inst;
    if (branch.isConditional()) {
      std::string instruction = "BRANCH_IF " + getReg(branch.getCondition()) + ", targetLabel";
      emitInstruction(instruction);
    } else {
      llvm::BasicBlock* bb = branch.getSuccessor(0);
      if (bbToAddr.count(bb)) {
        inst << "JMP " << bbToAddr.at(bb);
      } else {
        inst << "JMP ";
        needToRelocate.emplace_back(instructionCounter, bb);
      }
      emitInstruction(inst.str());
    }
  }

  // R0 is return register
  void translateCallInst(llvm::CallInst &call) {
    // TODO: struct ABI
    // temporary: push function arguments onto the stack
    llvm::Function* function = call.getFunction();
    for (auto & arg : function->args()) {
      emitInstruction("PUSH " + getReg(&arg));
    }
    if (!function->getReturnType()->isVoidTy()) {
      emitInstruction("MOV " + getReg(&call) + "R0");
    }
    // clear args
    std::string instruction = "CALL ";
    needToRelocate.emplace_back(instructionCounter, &function->getEntryBlock());
    emitInstruction(instruction);
  }

  // Emit a translated instruction
  void emitInstruction(const std::string &instruction) {
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
