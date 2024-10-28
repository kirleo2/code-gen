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
          _last_result = llvm::ConstantInt::get(getType(Symbol::KwInt), ast->value);
//            (*this) += LDI(RegType::Int, ast->value, ast);
        }

        void visit(ASTDouble * ast) override {
          _last_result = llvm::ConstantFP::get(getType(Symbol::KwDouble), ast->value);
//            (*this) += LDF(RegType::Float, ast->value, ast);
        }

        void visit(ASTChar * ast) override {
          _last_result = llvm::ConstantInt::get(getType(Symbol::KwChar), ast->value);
//            (*this) += LDI(RegType::Int, ast->value, ast);
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
            ASSERT(local.has_value());
            _last_result = local->value;
            if (lValue_)
                lValue_ = false;
            else
              _last_result = new llvm::LoadInst(local->type, local->value, ast->name.name(), _bb);
        }


        void visit(ASTType* ast) override { MARK_AS_UNUSED(ast); UNREACHABLE;  }
        void visit(ASTPointerType* ast) override { MARK_AS_UNUSED(ast); UNREACHABLE; } 
        void visit(ASTArrayType* ast) override { MARK_AS_UNUSED(ast); UNREACHABLE; } 
        void visit(ASTNamedType* ast) override {
          _last_result = (llvm::Value*) getType(ast->name);
        }

        /** Seuquence simply translates its elements. 
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
                translateAST(i.get());
            leaveBlock();
        }

        void visit(ASTVarDecl* ast) override {
          auto t = (llvm::Type*)translateAST(ast->varType.get());
          auto val = ast->value != nullptr ? translateAST(ast->value.get()) : nullptr;
          // We need to distinguish whether we are in the global context or inside the function
          if (_func == nullptr) {
            _last_result = new llvm::GlobalVariable(*_module, t, false, llvm::GlobalValue::ExternalLinkage,
                                                    (llvm::Constant*)val, ast->name->name.name());
          } else {
            _last_result = new llvm::AllocaInst(t, 0, ast->name->name.name(), _bb);
          }
          pushLocal(ast->name->name, _last_result, t);
          if (_func != nullptr && ast->value != nullptr) {
            // generate store if value present and we are at local context
            new llvm::StoreInst(val, _last_result, false, _bb);
          }
          _module->print(llvm::outs(), nullptr);
        }

        
        /** Enter a new function.
         */
        void visit(ASTFunDecl* ast) override {
          std::vector<llvm::Type*> types;
          // set up function parameters and return type
          for (auto & arg : ast->args) {
            auto type = translateAST(arg.first.get());
            types.push_back((llvm::Type*)type);
          }
          auto retType = translateAST(ast->returnType.get());
          llvm::FunctionType* functionType = llvm::FunctionType::get((llvm::Type*) retType, types, false);
          _func = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, ast->name.name(), *_module);
          _func->setCallingConv(llvm::CallingConv::C);
          _bb = llvm::BasicBlock::Create(*_context, "prolog", _func);
          // iterate through arguments to store them in environment, set their names and retrieve their values
          size_t i = 0;
          for (auto & arg : _func->args()) {
            arg.setName(ast->args[i].second->name.name());
            auto alloca = new llvm::AllocaInst(types[i], 0, ast->args[i].second->name.name(), _bb);
            new llvm::StoreInst(&arg, alloca, false, _bb);
            pushLocal(ast->args[i].second->name, alloca, types[i]);
            i++;
          }
          visitChild(ast->body);
          _module->print(llvm::outs(), nullptr);
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
            MARK_AS_UNUSED(ast);
            UNREACHABLE;
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
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTBreak* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTContinue* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTReturn* ast) override {
          visitChild(ast->value.get());
          _last_result = llvm::ReturnInst::Create(*_context, _last_result, _bb);
        }
        
        void visit(ASTBinaryOp* ast) override {
          visitChild(ast->left);
          auto lhs = _last_result;
          visitChild(ast->right);
          auto rhs = _last_result;
            if (ast->op == Symbol::Add) {
              _last_result = llvm::BinaryOperator::CreateAdd(lhs, rhs, "sum", _bb);
//                (*this) += MUL(binaryResult(lhs, rhs), lhs, rhs, ast);
            } else if (ast->op == Symbol::BitAnd) {
              _last_result = llvm::BinaryOperator::CreateAnd(lhs, rhs, "and", _bb);
            } else if (ast->op == Symbol::BitOr) {
              _last_result = llvm::BinaryOperator::CreateOr(lhs, rhs, "or", _bb);
            } else if (ast->op == Symbol::Sub) {

            } else if (ast->op == Symbol::Mul) {

            } else if (ast->op == Symbol::Div) {

            } else if (ast->op == Symbol::Xor) {
              NOT_IMPLEMENTED;
            }
              else {
                NOT_IMPLEMENTED;
            }
        }

        void visit(ASTAssignment* ast) override {

            auto lvalue = translateLValue(ast->lvalue.get());
            auto value = translateAST(ast->value.get());
            _last_result = new llvm::StoreInst(value, lvalue, false, _bb);

//            (*this) += ST(lvalue, value, ast);
        }

        void visit(ASTUnaryOp* ast) override {
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTUnaryPostOp* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTAddress* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTDeref* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTIndex* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTMember* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTMemberPtr* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTCall* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTCast* ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
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
      llvm::Value* translateAST(AST* ast) {
        visitChild(ast);
        return _last_result;
        }

        template<typename T>
        typename std::enable_if<std::is_base_of<AST, T>::value, Instruction *>::type
        translate(std::unique_ptr<T> const & child) {
            visitChild(child.get());
            return lastResult_;
        }   


        llvm::Value * translateLValue(AST * child) {
            bool old = lValue_;
            lValue_ = true;
            visitChild(child);
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

        struct Local {
          Local() {}
          Local(llvm::Value * val, llvm::Type* type): value(val), type(type) {}
          llvm::Value * value = nullptr;
          llvm::Type * type = nullptr;
        };

        struct Context {
            std::unordered_map<Symbol, Instruction *> locals;
            BasicBlock * localsBlock = nullptr;

            Context(BasicBlock * locals):
                localsBlock{locals} {
            }

        }; // ASTToILTranslator::Context

        RegType registerTypeFor(Type * t) {
            return t == Type::getDouble() ? RegType::Float : RegType::Int;            
        }


        RegType binaryResult(Instruction * lhs, Instruction * rhs) {
            ASSERT(lhs->type == rhs->type && "We need identical types on lhs and rhs");
            return lhs->type;
        }

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
            _func = nullptr;
        }

        /** Enters new block. 


         */
        void enterBlock() {
          llvm::BasicBlock *bb = llvm::BasicBlock::Create(*_context, "body", _func);
          if (_bb->getTerminator() == nullptr) {
            llvm::BranchInst::Create(bb, _bb);
          }
          _bb = bb;
          _environments.emplace_back();
//            BasicBlock * bb = f_->addBasicBlock();
//
//            if (! bb_->terminated())
//                bb_->append(JMP(bb));
//            bb_ = bb;
//            _contexts.push_back(Context{bb_});
        }

        void leaveBlock() {
            _environments.pop_back();
        }

        llvm::Type* getType(const Symbol & type) {
          // try to load from cache
          auto it = _type_cache.find(type);
          if (it != _type_cache.end()) {
            return it->second;
          }
          llvm::Type* _type = nullptr;
          if (type == Symbol::KwChar) {
            _type = llvm::Type::getInt8Ty(*_context);
          } else if (type == Symbol::KwInt) {
            _type = llvm::Type::getInt32Ty(*_context);
          } else if (type == Symbol::KwDouble) {
            _type = llvm::Type::getDoubleTy(*_context);
          } else if (type == Symbol::KwVoid) {
            _type = llvm::Type::getVoidTy(*_context);
          } else if (type == Symbol::KwStruct) {
            NOT_IMPLEMENTED;
          } else {
            UNREACHABLE;
          }
          _type_cache[type] = _type;
          return _type;
        }

        /** Creates new local variable with given name and size. The variable's ALLOCA instruction is appended to the current block's local definitions basic block and the register containing the address is returned. 
         */
        Instruction * addVariable(Symbol name, size_t size) {
            Instruction * res = _contexts.back().localsBlock->append(ALLOCA(RegType::Int, static_cast<int64_t>(size), name.name()));
            _contexts.back().locals.insert(std::make_pair(name, res));
            return res;
        }

        void pushLocal(Symbol name, llvm::Value* val, llvm::Type* type) {
          _environments.back().insert(std::make_pair(name, Local(val, type)));
        }

        /** Returns the register that holds the address of variable with given name. The address can then be used to load/store its contents. 
         */
        std::optional<Local> getLocal(Symbol name) {
            for (size_t i = _environments.size() - 1, e = _environments.size(); i < e; --i) {
                auto it = _environments[i].find(name);
                if (it != _environments[i].end())
                    return it->second;
            }
            return std::nullopt;
        }

        llvm::Module* _module = nullptr;
        llvm::LLVMContext* _context = nullptr;
        llvm::BasicBlock* _bb = nullptr;
        llvm::Value* _last_result = nullptr;
        llvm::Function * _func = nullptr;
        std::vector<std::unordered_map<Symbol, Local>> _environments;

        std::unordered_map<Symbol, llvm::Type*> _type_cache;

        Program p_;
        std::vector<Context> _contexts;
        Instruction * lastResult_ = nullptr;
        BasicBlock * bb_ = nullptr;
        Function * f_ = nullptr;
        bool lValue_ = false;


    }; // tiny::ASTToILTranslator

} // namespace tiny