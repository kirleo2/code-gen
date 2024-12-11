#pragma once


#include <unordered_map>
#include <memory>

#include "common/colors.h"
#include "frontend/ast.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

namespace tiny {
  struct Program {
    std::shared_ptr<llvm::LLVMContext> ctx;
    std::shared_ptr<llvm::Module> module;
  };
} // namespace tiny

