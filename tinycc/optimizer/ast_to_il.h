#pragma once

#include "common/types.h"
#include "frontend/ast.h"
#include "il.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/BasicBlock.h>
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"


namespace tiny {

    class ASTToILTranslator : public ASTVisitor {

    private:
      void define_intrinsics() {
        Type * scan = Type::getFunction(std::vector{Type::getChar()});
        Type * print_char = Type::getFunction(std::vector{Type::getVoid(), Type::getChar()});
        Type * print_int = Type::getFunction(std::vector{Type::getVoid(), Type::getInt()});

        putchar = llvm::Function::Create(llvm::dyn_cast<llvm::FunctionType>(getLLVMType(print_char)),
                  llvm::Function::ExternalLinkage, "putchar", *_module);
        pushLocal(Symbol{"printc"}, putchar, print_char);

        putint = llvm::Function::Create(llvm::dyn_cast<llvm::FunctionType>(getLLVMType(print_int)),
                                         llvm::Function::ExternalLinkage, "putint", *_module);
        pushLocal(Symbol{"printi"}, putint, print_int);

        getchar = llvm::Function::Create(llvm::dyn_cast<llvm::FunctionType>(getLLVMType(scan)),
                                         llvm::Function::ExternalLinkage, "getchar", *_module);
        pushLocal(Symbol{"scan"}, getchar, scan);
      }
    public:

      ASTToILTranslator() {
        program.ctx = std::make_shared<llvm::LLVMContext>();
        program.module = std::make_shared<llvm::Module>("module", *program.ctx);
        _context = program.ctx.get();
        _module = program.module.get();
        _environments.emplace_back();
        define_intrinsics();
      }

      ~ASTToILTranslator() {
        _module->print(llvm::outs(), nullptr);
//        delete _module;
      }

        static Program translateProgram(std::unique_ptr<AST> const & root) {
            ASTToILTranslator t;
            t.translate(root);
            return std::move(t.program);
        }

        void visit(AST * ast) override { 
            MARK_AS_UNUSED(ast); 
            UNREACHABLE;
        }

        /** Translating the program simply means translating all its statements in order. 
         */
        void visit(ASTProgram * ast) override {
            for (auto & s : ast->statements)
                translate(s);
        }
        
        /** Translating the literals is trivial. Since for simplicity reasons we do not allow literals to appear as arguments of operator instructions, each literal has to be first loaded as an immediate value to a new register. 
         */
        void visit(ASTInteger * ast) override {
          _last_result = llvm::ConstantInt::get(getLLVMType(ast->type()), ast->value);
        }

        void visit(ASTDouble * ast) override {
          _last_result = llvm::ConstantFP::get(getLLVMType(ast->type()), ast->value);
        }

        void visit(ASTChar * ast) override {
          _last_result = llvm::ConstantInt::get(getLLVMType(ast->type()), ast->value);
        }

        /** Translating string literals is a bit harder - each string literal is deduplicated and stored as a new global variable that is also initialized with the contents of the literal. 
         */
        void visit(ASTString* ast) override {
          auto str = llvm::ConstantDataArray::getString(*_context, ast->value, true);
          _last_result = new llvm::GlobalVariable(*_module, getLLVMType(ast->type()), true, llvm::GlobalValue::ExternalLinkage,
                                                  str, "string");
        }

        /** Identifier is translated as a variable read. Note that this is as the address. 
        */
        void visit(ASTIdentifier* ast) override {
          auto local = getLocal(ast->name);
          _last_result = local.first;
          std::cout << "Translating ASTIdentifier name: " << ast->name << std::endl;
          if (lValue_) {
            lValue_ = false;
          } else {
            ASSERT(llvm::isa<llvm::AllocaInst>(local.first));
            _last_result = new llvm::LoadInst(getLLVMType(ast->type()), _last_result, ast->name.name(), _bb);
          }
        }

        void visit(ASTType* ast) override { MARK_AS_UNUSED(ast); UNREACHABLE;  }
        void visit(ASTPointerType* ast) override { MARK_AS_UNUSED(ast); UNREACHABLE; }
        void visit(ASTArrayType* ast) override {
          // TODO: better approach, now we translate the size to propagate it into ASTVarDecl
          _last_result = translate(ast->size);
          ASSERT(_last_result->getType()->isIntegerTy());
        }
        void visit(ASTNamedType* ast) override { MARK_AS_UNUSED(ast); UNREACHABLE; }

        /** Sequence simply translates its elements.
         */
        void visit(ASTSequence* ast) override { 
            for (auto & i : ast->body)
              visitChild(i);
        }

        /** */
        void visit(ASTBlock* ast) override { 
            ASSERT(_func != nullptr);
            enterBlock();
            for (auto & i : ast->body) 
                visitChild(i.get());
            leaveBlock();
        }

        void visit(ASTVarDecl* ast) override {
          // Hack with arrays: We treat them as usual pointers during typechecking,
          // but we want to transtale it to array allocation here in IR phase, so
          // we need this special check
          llvm::Value* alloc_size = nullptr;
          llvm::Type* t = nullptr;
          if (ast->varType->type()->isArray()) {
            // global arrays are not implemented
            ASSERT(_func);
            alloc_size = translate(ast->varType);
            t = getLLVMType(dynamic_cast<PointerType*>(ast->varType->type())->base());
          } else {
            alloc_size = llvm::ConstantInt::get(getLLVMType(Type::getInt()), 1);
            t = getLLVMType(ast->varType->type());
          }
          auto val = ast->value != nullptr ? translate(ast->value) : nullptr;
          // We need to distinguish whether we are in the global context or inside the function
          if (_func == nullptr) {
            _last_result = new llvm::GlobalVariable(*_module, t, false, llvm::GlobalValue::ExternalLinkage,
                                                    (llvm::Constant*)val, ast->name->name.name());
          } else {
            _last_result = new llvm::AllocaInst(t, 0, alloc_size, ast->name->name.name(), _bb);
          }
          pushLocal(ast->name->name, _last_result, ast->type());
          if (_func != nullptr && ast->value != nullptr) {
            // generate store if value present and we are at local context
            new llvm::StoreInst(val, _last_result, false, _bb);
          }
        }

        
        /** Enter a new function.
         */
        void visit(ASTFunDecl* ast) override {
          auto functionType = llvm::cast<llvm::FunctionType>(getLLVMType(ast->type()));
          _func = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, ast->name.name(), *_module);
          _func->setCallingConv(llvm::CallingConv::C);
          pushLocal(ast->name, _func, ast->type());
          _bb = llvm::BasicBlock::Create(*_context, "prolog", _func);
          _environments.emplace_back();
          // iterate through arguments to store them in environment, set their names and retrieve their values
          size_t i = 0;
          for (auto & arg : _func->args()) {
            arg.setName(ast->args[i].second->name.name());
            assert(getLLVMType(dynamic_cast<FunctionType*>(ast->type())->arg(i)) == arg.getType());
            auto alloca = new llvm::AllocaInst(arg.getType(), 0, ast->args[i].second->name.name(), _bb);
            new llvm::StoreInst(&arg, alloca, false, _bb);
            pushLocal(ast->args[i].second->name, alloca, dynamic_cast<FunctionType*>(ast->type())->arg(i));
            i++;
          }
          translate(ast->body);
          leaveFunction();
        }

        /** Nothing to do for a struct declaration in the translation phase, the type has been created by the typechecker already.
         */ 
        void visit(ASTStructDecl* ast) override { MARK_AS_UNUSED(ast); _last_result = nullptr; }

        /** Nothing to do for a function ptr declaration in the translation phase, the type has already been created by the typechecker.
         */
        void visit(ASTFunPtrDecl* ast) override {MARK_AS_UNUSED(ast); _last_result = nullptr; }

        void visit(ASTIf* ast) override {
          auto cmp = translate(ast->cond);
          auto if_exit = llvm::BasicBlock::Create(*_context, "if_exit", _func);
          auto true_branch = llvm::BasicBlock::Create(*_context, "true", _func);
          auto false_branch = ast->falseCase !=  nullptr ? llvm::BasicBlock::Create(*_context, "false", _func) : if_exit;
          llvm::BranchInst::Create(true_branch, false_branch, cmp, _bb);
          _bb = true_branch;
          visitChild(ast->trueCase.get());
          llvm::BranchInst::Create(if_exit, _bb);
          if (ast->falseCase != nullptr) {
            _bb = false_branch;
            visitChild(ast->falseCase.get());
            llvm::BranchInst::Create(if_exit, _bb);
          }
          _bb = if_exit;
        }

        void visit(ASTSwitch* ast) override {
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTWhile* ast) override {
          auto cond = llvm::BasicBlock::Create(*_context, "loop_condition", _func);
          auto loop_body = llvm::BasicBlock::Create(*_context, "loop_body", _func);
          auto loop_exit = llvm::BasicBlock::Create(*_context, "loop_exit", _func);

          auto old_break = _break_target;
          auto old_continue = _continue_target;

          llvm::BranchInst::Create(cond, _bb);
          auto cmp = translate(ast->cond);
          llvm::BranchInst::Create(loop_body, loop_exit, cmp, _bb);

          _bb = loop_body;
          translate(ast->body);

          _bb = loop_exit;
          // restore break and continue targets
          _break_target = old_break;
          _continue_target = old_continue;
        }

        void visit(ASTDoWhile* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;

        }

        void visit(ASTFor* ast) override {
          // prepare BB for condition, increment, body, and exit and save break, continue targets
          auto cond = llvm::BasicBlock::Create(*_context, "loop_condition", _func);
          auto loop_body = llvm::BasicBlock::Create(*_context, "loop_body", _func);
          auto loop_exit = llvm::BasicBlock::Create(*_context, "loop_exit", _func);
          auto loop_inc = llvm::BasicBlock::Create(*_context, "loop_inc", _func);

          auto old_break = _break_target;
          auto old_continue = _continue_target;
          _break_target = loop_exit;
          _continue_target = loop_inc;
          // initialize loop (may be NULL)
          if (ast->init) translate(ast->init);
          llvm::BranchInst::Create(cond, _bb);
          // start loop with condition check, may be NULL
          _bb = cond;
          if (ast->cond) {
            auto cmp = translate(ast->cond);
            llvm::BranchInst::Create(loop_body, loop_exit, cmp, _bb);
          } else {
            // if NULL we do jump always
            llvm::BranchInst::Create(loop_body, _bb);
          }
          // translate body
          ASSERT(ast->body);
          _bb = loop_body;
          translate(ast->body);
          llvm::BranchInst::Create(loop_inc, _bb);
          _bb = loop_inc;
          // may be NULL
          if (ast->increment) translate(ast->increment);
          llvm::BranchInst::Create(cond, _bb);
          _bb = loop_exit;
          // restore break and continue targets
          _break_target = old_break;
          _continue_target = old_continue;
        }

        void visit(ASTBreak* ast) override {
          MARK_AS_UNUSED(ast);
          ASSERT(_break_target != nullptr);
          llvm::BranchInst::Create(_break_target, _bb);
        }

        void visit(ASTContinue* ast) override {
          MARK_AS_UNUSED(ast);
          ASSERT(_continue_target != nullptr);
          llvm::BranchInst::Create(_continue_target, _bb);
        }

        void visit(ASTReturn* ast) override {
          visitChild(ast->value.get());
          _last_result = llvm::ReturnInst::Create(*_context, _last_result, _bb);
        }
        
        void visit(ASTBinaryOp* ast) override {
          std::cout << "Translating binop " << ast->op << std::endl;
          // Always translate lhs to current BB
          llvm::Value* lhs = createCast(translate(ast->left), getLLVMType(ast->type()));
          llvm::Value* rhs = nullptr;
          // Operators && and || need special treatment,
          // because we do not want to evaluate both of conditionds
          // always and we want to use PHI node to choose control flow
          if (ast->op == Symbol::And || ast->op == Symbol::Or) {
            // Create BB for rhs condition and evaluate it inside, because we want to jump there based
            // on the lhs condition
            llvm::BasicBlock* cond_rhs = llvm::BasicBlock::Create(*_context,"cond_rhs", _func);
            _bb = cond_rhs;
            rhs = createCast(translate(ast->right), getLLVMType(ast->type()));

            // Intermediate represents evaluation result of lhs. False in case of && and true in case of ||.
            llvm::BasicBlock* intermediate = llvm::BasicBlock::Create(*_context, "intermediate", _func);

            // End of operator
            llvm::BasicBlock* end = llvm::BasicBlock::Create(*_context, "end_of_logical_binop", _func);
            llvm::BranchInst::Create(end, cond_rhs);
            llvm::BranchInst::Create(end, intermediate);

            // PHI Node represents the control flow
            llvm::PHINode* result = llvm::PHINode::Create(getLLVMType(ast->type()), 2, "result", end);

            result->addIncoming(lhs, intermediate);
            result->addIncoming(rhs, cond_rhs);

            if (ast->op == Symbol::And) {
              // Do not evaluate cond_rhs if lhs is false
              llvm::BranchInst::Create(cond_rhs, intermediate, lhs, _bb);
            } else {
              // Do not evaluate cond_rhs if lhs is true
              llvm::BranchInst::Create(intermediate, cond_rhs, lhs, _bb);
            }

            // exit evaluation
            _bb = end;
          } else {
            rhs = createCast(translate(ast->right), getLLVMType(ast->type()));
            if (ast->op == Symbol::Add) {
              if (ast->type() == Type::getDouble()) {
                _last_result = llvm::BinaryOperator::CreateFAdd(lhs, rhs, "fadd", _bb);
              } else {
                _last_result = llvm::BinaryOperator::CreateAdd(lhs, rhs, "add", _bb);
              }
            } else if (ast->op == Symbol::BitAnd) {
              _last_result = llvm::BinaryOperator::CreateAnd(lhs, rhs, "and", _bb);
            } else if (ast->op == Symbol::BitOr) {
              _last_result = llvm::BinaryOperator::CreateOr(lhs, rhs, "or", _bb);
            } else if (ast->op == Symbol::Sub) {
              if (ast->type() == Type::getDouble()) {
                _last_result = llvm::BinaryOperator::CreateFSub(lhs, rhs, "fsub", _bb);
              } else {
                _last_result = llvm::BinaryOperator::CreateSub(lhs, rhs, "sub", _bb);
              }
            } else if (ast->op == Symbol::Mul) {
              if (ast->type() == Type::getDouble()) {
                _last_result = llvm::BinaryOperator::CreateFMul(lhs, rhs, "fmul", _bb);
              } else {
                _last_result = llvm::BinaryOperator::CreateMul(lhs, rhs, "mul", _bb);
              }
            } else if (ast->op == Symbol::Div) {
              if (ast->type() == Type::getDouble()) {
                _last_result = llvm::BinaryOperator::CreateFDiv(lhs, rhs, "fdiv", _bb);
              } else {
                _last_result = llvm::BinaryOperator::CreateSDiv(lhs, rhs, "sdiv", _bb);
              }
            } else if (ast->op == Symbol::Mod)  {
              _last_result = llvm::BinaryOperator::CreateSRem(lhs, rhs, "mod", _bb);
            } else if (ast->op == Symbol::Xor) {
              _last_result = llvm::BinaryOperator::CreateXor(lhs, rhs, "xor", _bb);
            } else if (ast->op == Symbol::Lt) {
              if (ast->left->type() == Type::getDouble()) {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::FCmp, llvm::CmpInst::FCMP_OLT, lhs, rhs, "flt", _bb);
              } else {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::CmpInst::ICMP_SLT, lhs, rhs, "ilt", _bb);
              }
            } else if (ast->op == Symbol::Lte) {
              if (ast->left->type() == Type::getDouble()) {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::FCmp, llvm::CmpInst::FCMP_OLE, lhs, rhs, "lfte", _bb);
              } else {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::CmpInst::ICMP_SLE, lhs, rhs, "ilte", _bb);
              }
            } else if (ast->op == Symbol::Gt) {
              if (ast->left->type() == Type::getDouble()) {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::FCmp, llvm::CmpInst::FCMP_OGT, lhs, rhs, "fgt", _bb);
              } else {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::CmpInst::ICMP_SGT, lhs, rhs, "igt", _bb);
              }
            } else if (ast->op == Symbol::Gte) {
              if (ast->left->type() == Type::getDouble()) {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::FCmp, llvm::CmpInst::FCMP_OGE, lhs, rhs, "fge", _bb);
              } else {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::CmpInst::ICMP_SGE, lhs, rhs, "ige", _bb);
              }
            } else if (ast->op == Symbol::Eq) {
              if (ast->left->type() == Type::getDouble()) {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::FCmp, llvm::CmpInst::FCMP_OEQ, lhs, rhs, "feq", _bb);
              } else {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::CmpInst::ICMP_EQ, lhs, rhs, "ieq", _bb);
              }
            } else if (ast->op == Symbol::NEq) {
              if (ast->left->type() == Type::getDouble()) {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::FCmp, llvm::CmpInst::FCMP_ONE, lhs, rhs, "fneq", _bb);
              } else {
                _last_result = llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::CmpInst::ICMP_NE, lhs, rhs, "ineq", _bb);
              }
            } else if (ast->op == Symbol::ShiftRight) {
              _last_result = llvm::BinaryOperator::CreateAShr(lhs, rhs, "Ashr", _bb);
            } else if (ast->op == Symbol::ShiftLeft) {
              _last_result = llvm::BinaryOperator::CreateShl(lhs, rhs, "Shl", _bb);
            } else {
              std::cout << "OPERATOR:" << ast->op.name() << std::endl;
              UNREACHABLE;
            }
          }
        }

        void visit(ASTAssignment* ast) override {
          // lvalue should not fetch load instruction, we only need to obtain register from env
          auto lvalue = translateLValue(ast->lvalue);
          auto value = translate(ast->value);
          // implicit cast if necessary
          value = createCast(value, getLLVMType(ast->type()));
          _last_result = new llvm::StoreInst(value, lvalue, false, _bb);
        }

        void visit(ASTUnaryOp* ast) override {
          // fetch the var address, load the value, then apply Op and return after application
          llvm::Value* val = translateLValue(ast->arg);
          llvm::Value* load_val = new llvm::LoadInst(getLLVMType(ast->arg->type()), val, val->getName(), _bb);
          // implicit cast if necessary
          load_val = createCast(load_val, getLLVMType(ast->type()));

          if (ast->op == Symbol::Inc || ast->op == Symbol::Dec) {
            // The type could be pointer, int or double
            ASSERT(ast->type()->isNumeric() || ast->type()->isPointer());
            llvm::Value * constant = nullptr;

            if (ast->type() == Type::getDouble()) {
              constant = llvm::ConstantFP::get(getLLVMType(Type::getDouble()), ast->op == Symbol::Inc ? 1.0 : -1.0);
            } else {
              constant = llvm::ConstantInt::get(getLLVMType(Type::getInt()), ast->op == Symbol::Inc ? 1 : -1);
            }

            if (ast->type()->isNumeric()) {
              _last_result = llvm::BinaryOperator::CreateAdd(load_val, constant, "inc", _bb);
            } else {
              _last_result = llvm::GetElementPtrInst::Create(getLLVMType(dynamic_cast<PointerType*>(ast->arg->type())->base()), load_val, constant, "inc_ptr", _bb);
            }

            new llvm::StoreInst(_last_result, val, false, _bb);
          } else if (ast->op == Symbol::Neg) {
            llvm::Value *allOnes = llvm::ConstantInt::getAllOnesValue(llvm::Type::getInt32Ty(*_context));
            _last_result = llvm::BinaryOperator::CreateXor(load_val, allOnes, "~int", _bb);
          } else if (ast->op == Symbol::Sub) {
            // for double -> FNEG
            // for int 0 -> value
            if (ast->type() == Type::getDouble()) {
              _last_result = llvm::UnaryOperator::CreateFNeg(load_val, "fneg", _bb);
            } else {
              ASSERT(ast->type() == Type::getInt());
              llvm::Value *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*_context), 0);
              _last_result = llvm::BinaryOperator::CreateSub(zero, val, "neg", _bb);
            }
          }
        }

        void visit(ASTUnaryPostOp* ast) override {
          // fetch the var address, load the value, then apply Op and return loaded value before application
          llvm::Value* val = translateLValue(ast->arg);
          llvm::Value* load_val = new llvm::LoadInst(getLLVMType(ast->arg->type()), val, val->getName(), _bb);

          load_val = createCast(load_val, getLLVMType(ast->type()));

          if (ast->op == Symbol::Inc || ast->op == Symbol::Dec) {
            // for each type increment should be different:
            // we have to obtain the type and emit appropriate constant. In case of pointers
            // we have to emit constant equals to pointee size;

            // The type could be pointer, int or double
            ASSERT(ast->type()->isNumeric() || ast->type()->isPointer());
            llvm::Value * constant = nullptr;

            if (ast->type() == Type::getDouble()) {
              constant = llvm::ConstantFP::get(getLLVMType(Type::getDouble()), ast->op == Symbol::Inc ? 1.0 : -1.0);
            } else {
              constant = llvm::ConstantInt::get(getLLVMType(Type::getInt()), ast->op == Symbol::Inc ? 1 : -1);
            }
            // tmp variable to discard the result
            llvm::Value* res = nullptr;
            if (ast->type()->isNumeric()) {
              res = llvm::BinaryOperator::CreateAdd(load_val, constant, "inc", _bb);
            } else {
              res = llvm::GetElementPtrInst::Create(getLLVMType(dynamic_cast<PointerType*>(ast->arg->type())->base()), load_val, constant, "inc_ptr", _bb);
            }

            new llvm::StoreInst(res, val, false, _bb);
          } else {
            UNREACHABLE;
          }

          _last_result = load_val;
        }

        void visit(ASTAddress* ast) override {
          // just fetch the lvalue, it will be a pointer to allocated memory
          _last_result = translateLValue(ast->target);
        }

        llvm::Value* dereference(AST * ast, AST * index = nullptr) {
          ASSERT(ast->type()->isPointer());
          // Do not propogate current lvalue deeper
          llvm::Value* result = nullptr;
          bool old_lvalue = lValue_;
          lValue_ = false;

          // load the pointer value
          visitChild(ast);
          llvm::Value* ptr = _last_result;
          PointerType* ptr_type = dynamic_cast<PointerType*>(ast->type());

          if (index != nullptr) {
            visitChild(index);
            ASSERT(_last_result->getType()->isIntegerTy());
            ptr = llvm::GetElementPtrInst::Create(getLLVMType(ptr_type->base()), ptr, {_last_result}, "ptr+index", _bb);
          }

          // apply current Lvalue
          lValue_ = old_lvalue;

          // load the pointee value
          if (lValue_) {
            lValue_ = false;
            result = ptr;
          } else {
            result = new llvm::LoadInst(getLLVMType(ptr_type->base()), _last_result, "*(ptr + index)", _bb);
          }
          return result;
        }

        void visit(ASTDeref* ast) override {
          _last_result = dereference(ast->target.get());
        }

        void visit(ASTIndex* ast) override {
          _last_result = dereference(ast->base.get(), ast->index.get());
        }

        llvm::Value* extractMember(std::unique_ptr<AST> & base, const Symbol & member, bool deref=false) {
          // Do not propogate current lvalue deeper
          llvm::Value* result = nullptr;
          bool old_lvalue = lValue_;
          lValue_ = false;

          std::cout << "Member: " << member << std::endl;
          llvm::Value* struct_ = nullptr;
          if (deref) {
            struct_ = translate(base);
          } else {
            struct_ = translateLValue(base);
          }
          assert(struct_ != nullptr);
          ASSERT(struct_->getType()->isPointerTy());
          StructType* structType = dynamic_cast<StructType*>(base->type());

          // apply current Lvalue
          lValue_ = old_lvalue;

          // get the pointer the member and load its value
          for (size_t i = 0; i < structType->fieldCount(); i++) {
            if ((*structType)[i].first == member) {
              result = llvm::GetElementPtrInst::Create(getLLVMType(base->type()), struct_,
                                                             {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*_context), 0),
                                                              llvm::ConstantInt::get(llvm::Type::getInt32Ty(*_context), i)}, ".member", _bb);
              assert(result != nullptr);
              if (lValue_) {
                lValue_ = false;
              } else {
                result = new llvm::LoadInst(getLLVMType((*structType)[i].second), result, member.name(), _bb);
              }
              return result;
            }
          }

          UNREACHABLE;
          return result;
        }
        void visit(ASTMember* ast) override {
          _last_result = extractMember(ast->base, ast->member);
        }

        void visit(ASTMemberPtr* ast) override {
          _last_result = extractMember(ast->base, ast->member, true);
        }

        void visit(ASTCall* ast) override {
          llvm::Value* func = nullptr;
          if (ast->function->type()->isPointer()) {
            func = translate(ast->function);
          } else {
            func = translateLValue(ast->function);
          }
          ASSERT(llvm::isa<llvm::Function>(func));
          std::vector<llvm::Value*> args;
          for (const auto & arg: ast->args) {
            args.push_back(translate(arg));
          }
          _last_result = llvm::CallInst::Create(llvm::cast<llvm::Function>(func)->getFunctionType(), func, args, "call", _bb);
        }

        /* Does cast if types are different and this is possible */
        llvm::Value* createCast(llvm::Value * src, llvm::Type * type) {
          // if src->type != type, create an appropriate cast instruction from src to dest
          llvm::Value* casted = src;
          llvm::Type* src_type = src->getType();
          if (src_type != type) {
            if (src_type->isIntegerTy() && type->isIntegerTy()) {
              // Integer to integer: Extend or truncate
              unsigned SrcBits = src_type->getIntegerBitWidth();
              unsigned DestBits = type->getIntegerBitWidth();
              if (SrcBits < DestBits) {
                casted = new llvm::SExtInst(src, type, "sext", _bb);
              } else {
                casted = new llvm::TruncInst(src, type, "trunc", _bb);
              }
            } else if (src_type->isIntegerTy() && type->isDoubleTy()) {
              // TODO: Handle char?
              casted = new llvm::SIToFPInst(src, type, "sitofp", _bb);
            } else if (src_type->isIntegerTy() && type->isPointerTy()) {
              // TODO: Handle char?
              casted = new llvm::IntToPtrInst(src, type, "inttoptr", _bb);
            } else if (src_type->isPointerTy() && type->isIntegerTy()) {
              // TODO: Handle char?
              casted = new llvm::PtrToIntInst(src, type, "ptrtoint", _bb);
            } else if (src_type->isPointerTy() && type->isPointerTy()) {
            // Pointer to pointer: Bitcast
            // TODO: How does it work with opaque pointers?
            casted = new llvm::BitCastInst(src, type, "bitcast", _bb);
          } else {
              // Unsupported cast
              src_type->print(llvm::outs());
              type->print(llvm::outs());
              UNREACHABLE;
            }
          }
          return casted;
        }

        void visit(ASTCast* ast) override {
          createCast(translate(ast->value), getLLVMType(ast->type->type()));
        }

        void visit(ASTPrint* ast) override {
          llvm::Function * intrinsic = nullptr;
          std::string name;
          if (ast->value->type() == Type::getChar()) {
            intrinsic = putchar;
            name = "putchar";
          } else {
            intrinsic = putint;
            name = "putint";
          }
          _last_result = llvm::CallInst::Create(intrinsic->getFunctionType(), intrinsic, {translate(ast->value)}, name, _bb);
        }

        void visit(ASTScan* ast) override {
          _last_result = llvm::CallInst::Create(getchar->getFunctionType(), getchar, {}, "getchar", _bb);
        }

    private:

        template<typename T>
        typename std::enable_if<std::is_base_of<AST, T>::value, llvm::Value*>::type
        translate(std::unique_ptr<T> const & child) {
            visitChild(child.get());
            return _last_result;
        }

      template<typename T>
      typename std::enable_if<std::is_base_of<AST, T>::value, llvm::Value*>::type
      translateLValue(std::unique_ptr<T> const & child) {
        bool old = lValue_;
        lValue_ = true;
        visitChild(child.get());
        lValue_ = old;
        return _last_result;
      }

        struct Context {
            std::unordered_map<Symbol, Instruction *> locals;
            BasicBlock * localsBlock = nullptr;

            Context(BasicBlock * locals):
                localsBlock{locals} {
            }

        }; // ASTToILTranslator::Context


        void leaveFunction() {
            _environments.pop_back();
            _func = nullptr;
            if (_bb->getTerminator() == nullptr) {
              llvm::ReturnInst::Create(*_context, _bb);
            }
            _bb = nullptr;
        }

        /** Enters new block.
         */
        void enterBlock() {
          llvm::BasicBlock *bb = llvm::BasicBlock::Create(*_context, "block", _func);
          if (_bb->getTerminator() == nullptr) {
            llvm::BranchInst::Create(bb, _bb);
          }
          _bb = bb;
          _environments.emplace_back();
        }

        void leaveBlock() {
            _environments.pop_back();
        }

        llvm::Type* getLLVMType(Type * type) {
          // try to load from cache
          auto it = _type_cache.find(type);
          if (it != _type_cache.end()) {
            return it->second;
          }

          llvm::Type* llvm_type = nullptr;
          // type is SimpleType
          if (type == Type::getInt()) {
            llvm_type = llvm::Type::getInt32Ty(*_context);
          } else if (type == Type::getChar()) {
            llvm_type = llvm::Type::getInt8Ty(*_context);
          } else if (type == Type::getDouble()) {
            llvm_type = llvm::Type::getDoubleTy(*_context);
          } else if (type == Type::getVoid()) {
            llvm_type = llvm::Type::getVoidTy(*_context);
          } else if (type->isPointer()) {
            // distinguish between array and usual ptr

            llvm_type = llvm::PointerType::get(*_context, 0);
          } else if (type->isFunction()) {
            FunctionType * f_type = dynamic_cast<FunctionType*> (type);
            ASSERT(f_type != nullptr);
            std::vector<llvm::Type*> types;
            // set up function parameters and return type
            for (size_t i = 0; i < f_type->numArgs(); i++) {
              types.push_back(getLLVMType(f_type->arg(i)));
            }

            auto retType = getLLVMType(f_type->returnType());
            llvm_type = llvm::FunctionType::get(retType, types, false);
          } else if (type->isStruct()) {
            StructType *struct_type = dynamic_cast<StructType *> (type);
            ASSERT(struct_type != nullptr);
            std::vector<llvm::Type*> types;
            for (size_t i = 0; i < struct_type->fieldCount(); i++) {
              types.push_back(getLLVMType((*struct_type)[i].second));
            }
            llvm_type = llvm::StructType::get(*_context, types);
          } else {
            UNREACHABLE;
          }

          _type_cache[type] = llvm_type;
          return llvm_type;
        }

        void pushLocal(Symbol name, llvm::Value* val, Type* type) {
          _environments.back().insert(std::make_pair(name, std::make_pair(val, type)));
        }

        /** Returns the register that holds the address of variable with given name. The address can then be used to load/store its contents. 
         */
        std::pair<llvm::Value*, Type*> getLocal(Symbol name) {
            for (size_t i = _environments.size() - 1, e = _environments.size(); i < e; --i) {
                auto it = _environments[i].find(name);
                if (it != _environments[i].end())
                    return it->second;
            }
            UNREACHABLE;
            return {};
        }

        llvm::Module* _module = nullptr;
        llvm::LLVMContext* _context = nullptr;
        llvm::BasicBlock* _bb = nullptr;
        llvm::BasicBlock* _break_target = nullptr;
        llvm::BasicBlock* _continue_target = nullptr;
        llvm::Value* _last_result = nullptr;
        llvm::Function * _func = nullptr;
        std::vector<std::unordered_map<Symbol, std::pair<llvm::Value*, Type*>>> _environments;
        llvm::Function * putchar = nullptr;
        llvm::Function * putint = nullptr;
        llvm::Function * getchar = nullptr;

        std::unordered_map<Type*, llvm::Type*> _type_cache;

        bool lValue_ = false;

        Program program;


    }; // tiny::ASTToILTranslator

} // namespace tiny