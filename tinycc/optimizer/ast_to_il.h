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
    public:

      ASTToILTranslator() {
        _context = new llvm::LLVMContext();
        _module = new llvm::Module("my module", *_context);
        _environments.emplace_back();
      }

      ~ASTToILTranslator() {
        _module->print(llvm::outs(), nullptr);
        delete _module;
      }

        static Program translateProgram(std::unique_ptr<AST> const & root) {
            ASTToILTranslator t;
            t.translate(root);
            return std::move(t.p_); 
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
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        /** Identifier is translated as a variable read. Note that this is as the address. 
        */
        void visit(ASTIdentifier* ast) override {
          // TODO How to deal with structures?
          auto local = getLocal(ast->name);
          if (lValue_) {
            _last_result = local.first;
            lValue_ = false;
          } else {
            ASSERT(llvm::isa<llvm::AllocaInst>(local.first));
            _last_result = new llvm::LoadInst(getLLVMType(ast->type()), _last_result, ast->name.name(), _bb);
          }

        }

        void visit(ASTType* ast) override { MARK_AS_UNUSED(ast); UNREACHABLE;  }
        void visit(ASTPointerType* ast) override { MARK_AS_UNUSED(ast); UNREACHABLE; }
        void visit(ASTArrayType* ast) override {
          MARK_AS_UNUSED(ast); UNREACHABLE;
//          auto element_type = translateAST(ast->base.get());
//          auto size = llvm::dyn_cast<llvm::ConstantInt> (translateAST(ast->size.get()));
//          ASSERT(size != nullptr);
//          _last_result = (llvm::Value*) llvm::ArrayType::get((llvm::Type*)element_type, size->getZExtValue());
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
          auto t = getLLVMType(ast->type());
          auto val = ast->value != nullptr ? translate(ast->value) : nullptr;
          // We need to distinguish whether we are in the global context or inside the function
          if (_func == nullptr) {
            _last_result = new llvm::GlobalVariable(*_module, t, false, llvm::GlobalValue::ExternalLinkage,
                                                    (llvm::Constant*)val, ast->name->name.name());
          } else {
            _last_result = new llvm::AllocaInst(t, 0, ast->name->name.name(), _bb);
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

////            Function * f = enterFunction(ast->name);
//            for (size_t i = 0, e = ast->args.size(); i != e; ++i) {
//                Symbol name = ast->args[i].second->name;
//                Instruction *arg = ARG(registerTypeFor(ast->args[i].first->type()), static_cast<int64_t>(i), ast->args[i].first.get(), name.name());
//                // now we need to create a local copy of the value so that it acts as a variable
//                Type * t = ast->args[i].first->type();
//                if (t->isPointer() || t->isNumeric()) {
//                    Instruction * addr = addVariable(name, static_cast<int64_t>(ast->args[i].first->type()->size()));
//                    (*this) += ST(addr, arg);
//                } else {
//                    _contexts.back().locals.insert(std::make_pair(name, arg));
//                }
//            }
//            translate(ast->body);
//            if (! bb_->terminated())
//                (*this) += RET();
//          _last_result = llvm::ReturnInst::Create(_context, ReturnValue, _bb);
        }

        /** Nothing to do for a struct declaration in the translation phase, the type has been created by the typechecker already.
         */ 
        void visit(ASTStructDecl* ast) override { MARK_AS_UNUSED(ast); lastResult_ = nullptr; }

        /** Nothing to do for a function ptr declaration in the translation phase, the type has already been created by the typechecker.
         */
        void visit(ASTFunPtrDecl* ast) override {MARK_AS_UNUSED(ast); lastResult_ = nullptr; }

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
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
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
          // initialize loop
          visitChild(ast->init.get());
          llvm::BranchInst::Create(cond, _bb);
          // start loop with condition check
          _bb = cond;
          auto cmp = translate(ast->cond);
          llvm::BranchInst::Create(loop_body, loop_exit, cmp, _bb);
          // translate body
          _bb = loop_body;
          visitChild(ast->body.get());
          llvm::BranchInst::Create(loop_inc, _bb);
          _bb = loop_inc;
          visitChild(ast->increment.get());
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
          auto lhs = translate(ast->left);
          lhs = createCast(lhs, getLLVMType(ast->type()));
          auto rhs = translate(ast->right);
          rhs = createCast(rhs, getLLVMType(ast->type()));

          // TODO: Pointer arithmetic?
          if (ast->op == Symbol::Add) {
            _last_result = llvm::BinaryOperator::CreateAdd(lhs, rhs, "sum", _bb);
          } else if (ast->op == Symbol::BitAnd) {
            _last_result = llvm::BinaryOperator::CreateAnd(lhs, rhs, "and", _bb);
          } else if (ast->op == Symbol::BitOr) {
            _last_result = llvm::BinaryOperator::CreateOr(lhs, rhs, "or", _bb);
          } else if (ast->op == Symbol::Sub) {
            _last_result = llvm::BinaryOperator::CreateSub(lhs, rhs, "sub", _bb);
          } else if (ast->op == Symbol::Mul) {
            _last_result = llvm::BinaryOperator::CreateMul(lhs, rhs, "mul", _bb);
          } else if (ast->op == Symbol::Div) {
            _last_result = llvm::BinaryOperator::CreateSDiv(lhs, rhs, "sdiv", _bb);
          } else if (ast->op == Symbol::Xor) {
            _last_result = llvm::BinaryOperator::CreateXor(lhs, rhs, "xor", _bb);
          } else if (ast->op == Symbol::Lt) {
            _last_result = llvm::ICmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLT, lhs, rhs, "lt", _bb);
          } else if (ast->op == Symbol::Lte) {
            _last_result = llvm::ICmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLE, lhs, rhs, "lte", _bb);
          } else if (ast->op == Symbol::Gt) {
            _last_result = llvm::ICmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SGT, lhs, rhs, "gt", _bb);
          } else if (ast->op == Symbol::Gte) {
            _last_result = llvm::ICmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SGE, lhs, rhs, "gte", _bb);
          } else if (ast->op == Symbol::Eq) {
            _last_result = llvm::ICmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_EQ, lhs, rhs, "eq", _bb);
          } else if (ast->op == Symbol::NEq) {
            _last_result = llvm::ICmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, lhs, rhs, "neq", _bb);
          }  else if (ast->op == Symbol::And) {
            // TODO
          } else {
            std::cout << "OPERATOR:" << ast->op.name() << std::endl;
            UNREACHABLE;
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
          auto target = translateLValue(ast->target);
          auto base_index = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*_context), 0);
          _last_result = llvm::GetElementPtrInst::Create(target->getType(), target, {base_index}, "addr", _bb);
        }

        void visit(ASTDeref* ast) override {
          // do not know how to do.... with opaque pointer types
          auto target = translate(ast->target);
          _last_result = new llvm::LoadInst(target->getType(), target, "", _bb);
        }

        void visit(ASTIndex* ast) override {
          auto index = translate(ast->index);
          ASSERT(llvm::isa<llvm::Constant>(index));
          auto alloca = translateLValue(ast->base);
          _last_result = llvm::GetElementPtrInst::Create(llvm::cast<llvm::AllocaInst>(alloca)->getAllocatedType(), alloca, {index}, "addr", _bb);
        }

        void visit(ASTMember* ast) override {
          llvm::Value* struct_ = translateLValue(ast->base);
          ASSERT(struct_ != nullptr);
          StructType* structType = dynamic_cast<StructType*>(ast->base->type());
          // get the pointer the member and load its value
          for (size_t i = 0; i < structType->fieldCount(); i++) {
            if ((*structType)[i].first == ast->member) {
              auto ptr = llvm::GetElementPtrInst::Create(getLLVMType(ast->base->type()), struct_,
                                              {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*_context), 0),
                                               llvm::ConstantInt::get(llvm::Type::getInt32Ty(*_context), i)}, "GEP", _bb);
              assert(ptr != nullptr);
              _last_result = new llvm::LoadInst(getLLVMType((*structType)[i].second), ptr, ast->member.name(), _bb);
              return;
            }
          }
        }

        void visit(ASTMemberPtr* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTCall* ast) override {
          auto func = translateLValue(ast->function);
          ASSERT(llvm::isa<llvm::Function>(func));
          std::vector<llvm::Value*> args;
          for (const auto & arg: ast->args) {
            args.push_back(translate(arg));
          }
          llvm::CallInst::Create(llvm::cast<llvm::Function>(func)->getFunctionType(), func, args, "call", _bb);
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
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTScan* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
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
        return _last_result;
        lValue_ = old;
      }

        /** Adds the given instruction to the program, adding it to the current basic block, which should not be terminated. 
         */
        ASTToILTranslator & operator += (Instruction * ins) {
            ASSERT(bb_ != nullptr);
            bb_->append(ins);
            lastResult_ = ins;
            return *this;
        }

        struct Context {
            std::unordered_map<Symbol, Instruction *> locals;
            BasicBlock * localsBlock = nullptr;

            Context(BasicBlock * locals):
                localsBlock{locals} {
            }

        }; // ASTToILTranslator::Context

        Function * enterFunction(Symbol name) {
//            ASSERT(f_ == nullptr);
//            f_ = p_.addFunction(name);
//            Instruction * fReg = FUN(name, name.name());
//            p_.globals()->append(fReg);
//            bb_ = f_->addBasicBlock("prolog");
//            _contexts.push_back(Context{bb_});
//            _contexts.front().locals.insert(std::make_pair(name, fReg));
//            return f_;
        }

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

        /** Creates new local variable with given name and size. The variable's ALLOCA instruction is appended to the current block's local definitions basic block and the register containing the address is returned. 
         */
        Instruction * addVariable(Symbol name, size_t size) {
            Instruction * res = _contexts.back().localsBlock->append(ALLOCA(RegType::Int, static_cast<int64_t>(size), name.name()));
            _contexts.back().locals.insert(std::make_pair(name, res));
            return res;
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

        std::unordered_map<Type*, llvm::Type*> _type_cache;

        Program p_;
        std::vector<Context> _contexts;
        Instruction * lastResult_ = nullptr;
        BasicBlock * bb_ = nullptr;
        Function * f_ = nullptr;
        bool lValue_ = false;


    }; // tiny::ASTToILTranslator

} // namespace tiny