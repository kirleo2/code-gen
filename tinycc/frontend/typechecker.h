#pragma once

#include "common/types.h"
#include "common/source_error.h"
#include "ast.h"



namespace tiny {
  #define ASSERT_TYPE(cond, msg) \
    if (!(cond)) { \
        throw TypeError{STR(msg), ast->location()}; \
    }

    class TypeError : public SourceError {
    public:
        TypeError(std::string const & what, SourceLocation const & location):
            SourceError{"TypeError", what, location} {
        }
    };

    /** The typechecker is an AST visitor that walks the AST tree and assigns a type to every node. 

        Failure to assign a type is a type error.  
     */
    class Typechecker : public ASTVisitor {
    public:

        static void checkProgram(std::unique_ptr<AST> const & root) {
            Typechecker t;
            t.typecheck(root);
        }

        /** It's ok to leave this unimplemented, the generic AST visitor only exists for fallbacks cases which we do not use in the typechecker. 
        */
        void visit(AST * ast) override { MARK_AS_UNUSED(ast); UNREACHABLE; }
        
        void visit(ASTProgram * ast) override { 
            for (auto & s : ast->statements)
                typecheck(s);
            ast->setType(Type::getVoid());
        }

        void visit(ASTInteger * ast) override { 
            ast->setType(Type::getInt());
        }

        void visit(ASTDouble * ast) override {
            ast->setType(Type::getDouble());
        }
        
        void visit(ASTChar * ast) override { 
            ast->setType(Type::getChar());
        }
        
        void visit(ASTString * ast) override {
          ast->setType(Type::getPointerTo(Type::getChar()));
        }

        /** Verify the variable exists, raise an error if not, otherwise set type the type of the variable.
         */
        void visit(ASTIdentifier * ast) override { 
            Type * t = getVariable(ast->name);
            if (t == nullptr)
                throw TypeError{STR("Unknown variable " << ast->name.name()), ast->location()};
            ast->setType(t);
        }            
        
        /** Actually unreachable, handled by the special cases below.
         */
        void visit(ASTType * ast) override { MARK_AS_UNUSED(ast); UNREACHABLE; }

        /** Create the pointer type. 
         */
        void visit(ASTPointerType * ast) override { 
            Type * base = typecheck(ast->base);
            ast->setType(Type::getPointerTo(base));
        } 

        /** A bit hacky, we treat arrays equally to pointers, i.e. in tinyc the sizeof array will be sizeof ptr still.
         * A proper way would be to have an array type that would also keep the size with it and be convertible to a pointer.
         */
        void visit(ASTArrayType * ast) override {
          Type * base = typecheck(ast->base);
          typecheck(ast->size);
          PointerType* ptr_type = Type::getPointerTo(base);
          // hack to propagate 'array'
          ptr_type->setIsArray();
          ast->setType(ptr_type);
        }
        
        /** For a named type, we need it to be present in the known types, otherwise it is a failure. 
         */
        void visit(ASTNamedType * ast) override { 
            Type * t = Type::getType(ast->name);
            if (t == nullptr) 
                throw TypeError{STR("Unknown type " << ast->name), ast->location()};
            ast->setType(t);
        }

        /** Sequence typechecks its elements and unlike block makes sure its own type is that of the last element. 
         */
        void visit(ASTSequence * ast) override { 
            Type * t = nullptr;
            for (auto & i : ast->body)
                t = typecheck(i);
            ast->setType(t);    
        }
        
        /** Typechecks a block of statements. Typechecks each part of the block and returns the void type.
         */
        void visit(ASTBlock * ast) override { 
            enterBlock();
            for (auto const & child : ast->body)
                typecheck(child);
            ast->setType(Type::getVoid());
            leaveBlock();
        }
        
        /** Variable declaration gets the variable type, if there is a value specified for it ensures the value type corresponds to the variable type and then adds the variable to local context. If the variable already exists in current context (we allow shadowing), an error is raised. 
         */
        void visit(ASTVarDecl * ast) override { 
            Type * t = typecheck(ast->varType);
            if (!t->isFullyDefined())
                throw TypeError(STR("Type " << *t << " is not fully defined yet"), ast->location());
            if (ast->value != nullptr) {
                Type * valueType = typecheck(ast->value);
                // TODO: Why now implicit conversion?

                if (!valueType->convertsImplicitlyTo(t))
                    throw TypeError{STR("Value of type " << *valueType << " cannot be assigned to variable of type " << *t), ast->location()};
            }
            addVariable(ast->name->name, t, ast);
            ast->setType(t);
        }        

        /** Typechecking a function simply checks that the body typechecks well.
         */
        void visit(ASTFunDecl * ast) override { 
            // first we need to create the function type signature in the form retType..args. All types used there must be fully defined for the declaration to succeed
            Type * returnType = typecheck(ast->returnType);
            if (! returnType->isFullyDefined())
                throw TypeError{"Function return type must be fully defined", ast->returnType->location()};
            std::vector<Type*> signature;
            signature.push_back(returnType);
            for (auto & i : ast->args) {
                Type * argType = typecheck(i.first);
                if (! argType->isFullyDefined())
                    throw TypeError{"Function argument type must be fully defined", i.first->location()};
                signature.push_back(argType);
            }
            // create the type of the function and add a global variable of the type with the name of the function so that we can get a pointer to it easily
            Type * ftype = Type::getFunction(signature);
            addVariable(ast->name, ftype, ast);
            // now that the function type has been created, we can enter the function, add local variables for its arguments and then typecheck its body
            enterFunction(returnType);
            for (auto & i : ast->args) 
                addVariable(i.second->name, i.first->type(), i.second.get());
            // verify that the function actually returns the type it should have. For this we need the block to return the result of its  
            typecheck(ast->body);
            if (!returned_ && returnType != Type::getVoid()) 
                throw TypeError(STR("Not all paths of the function return " << *returnType), ast->location());
            ast->setType(ftype);
            leaveFunction();
        }

        void visit(ASTStructDecl * ast) override {
          StructType * struct_ = Type::getOrDeclareStruct(ast->name);
          for (auto & field: ast->fields ) {
            ASSERT_TYPE(struct_->addField(field.first->name, typecheck(field.second)), "Struct member names are not unique")
          }
          if (ast->isDefinition) struct_->markAsFullyDefined();
          ast->setType(struct_);
        }

        void visit(ASTFunPtrDecl * ast) override {
          std::vector<Type*> signature;
          signature.push_back(typecheck(ast->returnType));
          for (auto & i : ast->args) {
            signature.push_back(typecheck(i));
          }
          auto func_type = Type::getFunction(signature);
          auto type = Type::getPointerTo(func_type);
          // we should not add name to scope, rather create an alias to type
          Type::createAlias(ast->name->name, type);
          ast->setType(type);
        }

        void visit(ASTIf * ast) override {
            bool old = returned_;
            if (! typecheck(ast->cond)->convertsToBool())
                throw TypeError{STR("Condition must convert to bool, but " << *ast->cond->type() << " found"), ast->cond->location()};
            returned_ = false;
            typecheck(ast->trueCase);
            bool allReturn = returned_;
            if (ast->falseCase != nullptr) {
                returned_ = false;
                typecheck(ast->falseCase);
                allReturn &= returned_;
            } else {
                allReturn = false;
            }
            ast->setType(Type::getVoid());            
            returned_ = old || allReturn;
        }

        void visit(ASTSwitch * ast) override {
          bool old = returned_;
          bool allReturn = true;
          ASSERT_TYPE(typecheck(ast->cond)->convertsImplicitlyTo(Type::getInt()), "Statement requires expression of integer type");
          for (const auto & case_ : ast->cases) {
            returned_ = false;
            typecheck(case_.second);
            allReturn &= returned_;
          }
          if (ast->defaultCase == nullptr) allReturn = false;
          ast->setType(Type::getVoid());
          returned_ = old || allReturn;
        }

        void visit(ASTWhile * ast) override {
          auto cond = typecheck(ast->cond);
          typecheck(ast->body);
          ASSERT_TYPE(cond->convertsToBool(),
                      *cond << " is not contextually convertible to 'bool'");
          ast->setType(Type::getVoid());
        }

        void visit(ASTDoWhile * ast) override { 
            MARK_AS_UNUSED(ast);
            NOT_IMPLEMENTED;
        }

        void visit(ASTFor * ast) override {
          if (ast->init) typecheck(ast->init);
          if (ast->cond) {
            auto cond = typecheck(ast->cond);
            ASSERT_TYPE(cond->convertsToBool(),
                        *cond << " is not contextually convertible to 'bool'");
          }
          if (ast->increment) typecheck(ast->increment);
          typecheck(ast->body);
          ast->setType(Type::getVoid());
         }

        /** No typechecking here */
        void visit(ASTBreak * ast) override { 
            ast->setType(Type::getVoid());
        }

        /** No typechecking here */
        void visit(ASTContinue * ast) override { 
            ast->setType(Type::getVoid());
        }

        /** Typecheck the expression and then verify that the type corresponds to the function type. 
         */
        void visit(ASTReturn * ast) override { 
            Type * result = ast->value ? typecheck(ast->value) : Type::getVoid();
            Type * expectedReturnType = contexts_.back().returnType;
            // TODO: Why not implicitly coversion?
            if (result != expectedReturnType)
                throw TypeError{STR("Return type " << *result << " found, but " << *expectedReturnType << " found"), ast->location()};
            ast->setType(result);
            returned_ = true; // mark that we have successfully returned
        }

        /** Binary operators are not hard, just a bit of processing is required. We need to support all binary operators TinyC recognizes, i.e.: *, /, %, +, -, <<, >>, <, <=, >, >=, ==, !=, &, |, && and ||. 
         */
        void visit(ASTBinaryOp * ast) override {
          auto left  = typecheck(ast->left);
          auto right = typecheck(ast->right);

          ASSERT_TYPE((right->isNumeric() || right->isPointer())
          && (left->isNumeric() || left->isPointer()),
          "Binary operator is not supported for " << *left << "and" << *right);

          if (ast->op == Symbol::Mul || ast->op == Symbol::Add || ast->op == Symbol::Div || ast->op == Symbol::Sub) {
            ASSERT_TYPE(!left->isPointer() && !right->isPointer(),
                        "Arithmetic binary operator is not supported for pointers");
            ASSERT_TYPE(left->convertsImplicitlyTo(right) || right->convertsImplicitlyTo(left),
                        "Arithmetic binary operator is not supported for " << *left << "and " << *right);
            ast->setType(getArithmeticResult(left, right));
          } else if (ast->op == Symbol::Mod) {
            ASSERT_TYPE(left->convertsImplicitlyTo(Type::getInt()) && right->convertsImplicitlyTo(Type::getInt()),
                        "Mod operator is not supported for " << *left << "and " << *right);
            ast->setType(Type::getInt());
          } else if (ast->op == Symbol::ShiftLeft || ast->op == Symbol::ShiftRight) {
            ASSERT_TYPE(left->convertsImplicitlyTo(Type::getInt()) && right->convertsImplicitlyTo(Type::getInt()),
                        "Shift operator is not supported for " << *left << "and " << *right);
            ast->setType(Type::getInt());
          } else if (ast->op == Symbol::Lt || ast->op == Symbol::Lte || ast->op == Symbol::Eq ||
                  ast->op == Symbol::NEq || ast->op == Symbol::Gt || ast->op == Symbol::Gte ||
                  ast->op == Symbol::And || ast->op == Symbol::Or) {
            ASSERT_TYPE(left->convertsImplicitlyTo(right) || right->convertsImplicitlyTo(left),
                        "Comparison operator is not supported for " << *left << "and" << *right);
            ast->setType(Type::getInt());
          } else if (ast->op == Symbol::BitAnd || ast->op == Symbol::BitOr) {
            ASSERT_TYPE(left->convertsImplicitlyTo(Type::getInt()) && right->convertsImplicitlyTo(Type::getInt()),
                        "Bitwise operator is not supported for " << *left << "and " << *right);
            ast->setType(Type::getInt());
          }
        }

        /** We must make sure the left hand side of the assignment has an address. Then ensure that the types match, including any implicit conversions and is ok, set own type to that of the rhs.
        */
        void visit(ASTAssignment * ast) override {
          auto value = typecheck(ast->value);
          auto lvalue = typecheck(ast->lvalue);
          if (!ast->lvalue->hasAddress()) {
            throw TypeError{STR("Left hand side of assignment does not have an address! "), ast->location()};
          }
          if (!value->convertsImplicitlyTo(lvalue)) {
            throw TypeError{STR("Type mismatch found in ASSIGNMENT"), ast->location()};
          }
          ast->setType(ast->lvalue->type());
        }

        /** We have to handle correctly the types for all unary operators in TinyC, i.e. +, -, ~, !, ++ and --. 
         */       
        void visit(ASTUnaryOp * ast) override {
          auto t = typecheck(ast->arg);
          ASSERT_TYPE(!t->convertsImplicitlyTo(Type::getVoid()), "Invalid type for unary expression");
          if (ast->op == Symbol::Inc || ast->op == Symbol::Dec) {
            ASSERT_TYPE(ast->arg->hasAddress(), "Type must have an address");
          }
          if (ast->op == Symbol::Not) {
            ASSERT_TYPE(t->convertsImplicitlyTo(Type::getInt()), "Invalid type for unary expression ~, got " << *t);
            ast->setType(Type::getInt());
          } else {
            ast->setType(ast->arg->type());
          }
        }

        /** TinyC only has two post-operands, the post-increment and post-decrement. As they both modify the value, they require it to has an address. Only numeric types and pointers are supported. 
         */
        void visit(ASTUnaryPostOp * ast) override {
          auto t = typecheck(ast->arg);
          ASSERT_TYPE(ast->arg->hasAddress(), "Must have an address");
          ASSERT_TYPE(t->isNumeric() || t->isPointer(), "Must be whether pointer or numeric");
          ast->setType(t);
        }
        
        /** The interesting feature of the address operator is that not every value in tinyC has an address (only local variables do)
        */
        void visit(ASTAddress * ast) override {
          auto t = typecheck(ast->target);
          ASSERT_TYPE(ast->target->hasAddress(), "Adress operator value must have an address");
          ast->setType(Type::getPointerTo(t));
        }

        /** Only pointers can be dereferenced, in which case the result is their base type.
         */
        void visit(ASTDeref * ast) override {
          auto t = typecheck(ast->target);
          ASSERT_TYPE(t->isPointer(), "Dereferenced type must be a pointer, got " << *t);
          ast->setType(dynamic_cast<PointerType*>(t)->base());
        }

        /** Only pointers can be indexed.
         */
        void visit(ASTIndex * ast) override {
          auto base = typecheck(ast->base);
          auto index = typecheck(ast->index);
          ASSERT_TYPE(base->isPointer(), "Index operator base must be a pointer, got " << *base);
          ASSERT_TYPE(index->convertsImplicitlyTo(Type::getInt()),
                      "Index operator index must ne implicitly convertable to int, got " << *index);
          ast->setType(dynamic_cast<PointerType*>(base)->base());
        }

        void visit(ASTMember * ast) override { 
            StructType * baseType = dynamic_cast<StructType*>(typecheck(ast->base));
            if (baseType == nullptr)
                throw TypeError(STR("Cannot take field from a non-struct type " << *(ast->type())), ast->location());
            if (! baseType->isFullyDefined())
                throw TypeError(STR("Cannot take field from a not fully defined type " << *baseType), ast->location());
            Type * t = (*baseType)[ast->member];
            if (t == nullptr)
                throw TypeError(STR("Struct of type " << *baseType << " does not have field " << ast->member.name()), ast->location());
            ast->setType(t);
        }

        /** Similar to AST member, but we must make sure that where we take the field from is a pointer and dereference it first.
         * Represents struct->member */
        void visit(ASTMemberPtr * ast) override {
          ASSERT_TYPE(typecheck(ast->base)->isPointer(), "Type must be a pointer, got " << *ast->base->type());
          auto base_ptr = dynamic_cast<PointerType*>(ast->base->type());
          auto struct_ = dynamic_cast<StructType*>(base_ptr->base());
          ASSERT_TYPE(struct_ != nullptr, "Expected struct type, got " << *base_ptr->base());
          ASSERT(struct_->isFullyDefined());
          auto member = (*struct_)[ast->member];
          ASSERT_TYPE(member != nullptr, "Struct " << *struct_ << "does not contain member " << ast->member);
          ast->setType(member);
        }
        
        void visit(ASTCall * ast) override { 
          Type * t = typecheck(ast->function);
          FunctionType * ft = nullptr;
          if (t->isPointer()) {
              PointerType* ptr = dynamic_cast<PointerType*>(t);
              ft = dynamic_cast<FunctionType*>(ptr->base());
          } else {
            ft = dynamic_cast<FunctionType*>(t);
          }
          if (ft == nullptr)
            throw TypeError{STR("Expected function, but value of " << *t << " found"), ast->location()};
          if (ast->args.size() != ft->numArgs())
            throw TypeError{STR("Function of type " << *ft << " requires " << ft->numArgs() << " arguments, but " << ast->args.size() << " given"), ast->location()};
          for (size_t i = 0; i < ast->args.size(); ++i) {
            Type * argType = typecheck(ast->args[i]);
            if (argType != ft->arg(i))
              throw TypeError{STR("Type " << *(ft->arg(i)) << " expected for argument " << (i + 1) << ", but " << *argType << " found"), ast->args[i]->location()};
          }
          ast->setType(ft->returnType());
        }

        /** In C-like languages, from typechecking perspective, casting is really trivial.
         * Anything can typecheck to anything, as long as (a) the value typechecks and
         * (b) the target type is fully defined.
         */
        void visit(ASTCast * ast) override { 
            typecheck(ast->value);
            Type * target = typecheck(ast->type);
            if (! target->isFullyDefined())
                throw TypeError(STR("Cannot typecheck to incomplete type " << *target), ast->location());
            ast->setType(target);
        }

        void visit(ASTPrint * ast) override {
          auto type = typecheck(ast->value);
          if (type != Type::getChar() && type != Type::getInt())
            throw TypeError{STR("Write expects char, but" << *ast->value->type() << " found"), ast->location()};
          ast->setType(Type::getVoid());
        }

        /** Reading returns always a character.
         */
        void visit(ASTScan * ast) override { 
            ast->setType(Type::getChar());
        }

    protected:

        struct Context {
            Type * returnType;
            std::unordered_map<Symbol, Type *> locals;

            Context(Type * returnType = nullptr): returnType{returnType} {}
        }; 

        Typechecker() {
          contexts_.push_back(Context{nullptr}); // the global scope
          Type::resetTypeInformation();
          // add stdlib functions
          Type * scan = Type::getFunction(std::vector{Type::getChar()});
          Type * print_char = Type::getFunction(std::vector{Type::getVoid(), Type::getChar()});
          Type * print_int = Type::getFunction(std::vector{Type::getVoid(), Type::getInt()});
          // ok to pass nullptr here as ast, those are builtins and ast is only used for error reporting, there should be no errors here:)
          addVariable(Symbol{"scan"}, scan, /* ast */nullptr);
          addVariable(Symbol{"printc"}, print_char, /* ast */nullptr);
          addVariable(Symbol{"printi"}, print_int, /* ast */nullptr);
        }

        /** Enters a new function context. 
         * 
        */
        void enterFunction(Type * returnType) {
            contexts_.push_back(Context{returnType});
            returned_ = false;
        }

        void leaveFunction() {
            contexts_.pop_back();
        }

        void enterBlock() {
            contexts_.push_back(Context{contexts_.back().returnType});
        }

        void leaveBlock() {
            contexts_.pop_back();
        }

        Type * getArithmeticResult(Type * a, Type * b) {
            if (!a->isNumeric() || !b->isNumeric())
                return nullptr; // not numbers
            Type * d = Type::getDouble();
            Type * i = Type::getInt();
            if (a == d || b == d)
                return d;
            if (a == i || b == i)
                return i;
            // otherwise it has to be char
            return Type::getChar();
        }

        void addVariable(Symbol name, Type * t, AST * ast) {
            auto & ctx = contexts_.back();
            auto i = ctx.locals.find(name);
            if (i != ctx.locals.end())
                throw ParserError{STR("Variable " << name.name() << " already declared in current scope"), ast->location()};
            ctx.locals[name] = t;
        }

        Type * getVariable(Symbol name) {
            for (size_t i = contexts_.size() - 1, e = contexts_.size(); i < e; --i) {
                auto it = contexts_[i].locals.find(name);
                if (it != contexts_[i].locals.end())
                    return it->second;
            }
            return nullptr;
        }

        template<typename T> 
        typename std::enable_if<std::is_base_of<AST, T>::value, Type *>::type
        typecheck(std::unique_ptr<T> const & child) {
            visitChild(child.get());
            Type * result = child->type();
            if (result == nullptr)
                throw TypeError{"Unable to type expression", child->location()};
            return result;
        }   

        /** We use this to track that the AST subtree we just typechecked had a valid return on all its control flow paths.
         * This means we need to clear the flag in every statement and set the flag in return. Special handling
         * is necessary at junctions such as after if or switch.
         */
        bool returned_ = false;

        std::vector<Context> contexts_; 


    }; // tiny::Typechecker


} // namespace tiny
