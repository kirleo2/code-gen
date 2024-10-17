#pragma once

#include <unordered_set>

#include "common/symbol.h"
#include "ast.h"

namespace tiny {

    class Parser {
    public:

        static std::unique_ptr<AST> parseFile(std::string const &filename) {
            Parser p{Lexer::TokenizeFile(filename)};
            std::unique_ptr<AST> result{p.PROGRAM()};
            p.pop(Token::Kind::EoF);
            return result;
        }

        static std::unique_ptr<AST> parse(std::string const &source) {
            Parser p{Lexer::Tokenize(source, "")};
            std::unique_ptr<AST> result{p.PROGRAM()};
            p.pop(Token::Kind::EoF);
            return result;
        }

    protected:

        /** We need new position because when reverting, the tentative types names that were created *after* the savepoint must be unrolled as well.
         */
        class Position {
        private:
            friend class Parser;

            Position(size_t i, size_t typesSize) : i_{i}, typesSize_{typesSize} {
            }

            size_t i_;
            size_t typesSize_;
        };

        Parser(std::vector<Token> &&tokens) : tokens_{std::move(tokens)} {
        }

        Position position() const {
            return Position{i_, possibleTypesStack_.size()};
        }

        void revertTo(Position const &p) {
            i_ = p.i_;
            while (possibleTypesStack_.size() > p.typesSize_) {
                possibleTypes_.erase(possibleTypesStack_.back());
                possibleTypesStack_.pop_back();
            }
        }

        bool eof() const {
            return i_ == tokens_.size() - 1;
        }

        Token const &top() const {
            return tokens_[i_];
        }

        Token const &pop() {
            Token const &result = top();
            if (i_ != tokens_.size() - 1)
                ++i_;
            return result;
        }

        Token const &pop(Token::Kind kind) {
            if (top().kind() != kind)
                throw ParserError{STR("Expected " << kind << ", but " << top() << " found"), top().location()};
            return pop();
        }

        Token const &pop(Symbol symbol) {
            if ((top().kind() != Token::Kind::Identifier && top().kind() != Token::Kind::Operator) || top().valueSymbol() != symbol)
                throw ParserError{STR("Expected " << symbol << ", but " << top() << " found"), top().location()};
            return pop();
        }

        bool condPop(Token::Kind kind) {
            if (top().kind() != kind)
                return false;
            pop();
            return true;
        }

        bool condPop(Symbol symbol) {
            if ((top().kind() != Token::Kind::Identifier && top().kind() != Token::Kind::Operator) || top().valueSymbol() != symbol)
                return false;
            pop();
            return true;
        }

        Token const &peek(int offset) {
            int index = static_cast<int>(i_) + offset;
            if (index < 0 || index >= static_cast<int>(tokens_.size()))
                index = static_cast<int>(tokens_.size()) - 1;
            return tokens_[index];
        }

        /** Determines if given token is a valid user identifier.
         */
        bool isIdentifier(Token const & t) {
            if (t != Token::Kind::Identifier)
                return false;
            if (t == Symbol::KwBreak || t == Symbol::KwCase || t == Symbol::KwCast || t == Symbol::KwChar || t == Symbol::KwContinue || t == Symbol::KwDefault || t == Symbol::KwDo || t == Symbol::KwDouble || t == Symbol::KwElse || t == Symbol::KwFor || t == Symbol::KwIf || t == Symbol::KwInt || t == Symbol::KwReturn || t == Symbol::KwStruct || t == Symbol::KwSwitch || t == Symbol::KwTypedef || t == Symbol::KwVoid || t == Symbol::KwWhile)
                return false;
            return true;
        }

        /** \name Types Ambiguity

            tinyC shares the unfortunate problem of C and C++ grammars which makes it impossible to determine whether an expression is type declaration or an expression simply from the grammar. take for instance

                foo * a;

            Is this declaration of variable of name `a` with type `foo*`, or is this multiplication of two variables `foo` and `a`. Ideally this ambiguity should be solved at the grammar level such as introducing `var` keyword, or some such, but for educational purposes we have decided to keep this "feature" in the language.

            The way to fix this is to make the parser track all possible type names so that an identifier can be resolved as being either variable, or a type, thus removing the ambiguity.
         */

        std::unordered_set<Symbol> possibleTypes_;
        std::vector<Symbol> possibleTypesStack_;

        /** Returns true if given symbol is a type.

            Checks both own tentative records and the frontend's valid records.
         */
        bool isTypeName(Symbol name) const;

        /** Adds given symbol as a tentative typename.

            Note that same typename can be added multiple times for forward declared structs.
         */
        void addTypeName(Symbol name) {
            possibleTypes_.insert(name);
            possibleTypesStack_.push_back(name);
        }

        /*  Parsing

            Nothing fancy here, just a very simple recursive descent parser built on the basic framework.
         */
        std::unique_ptr<AST> PROGRAM();
        std::unique_ptr<AST> FUN_DECL();
        std::unique_ptr<AST> STATEMENT();
        std::unique_ptr<AST> BLOCK_STMT();
        std::unique_ptr<ASTIf> IF_STMT();
        std::unique_ptr<AST> SWITCH_STMT();
        std::unique_ptr<AST> CASE_BODY();
        std::unique_ptr<AST> WHILE_STMT();
        std::unique_ptr<AST> DO_WHILE_STMT();
        std::unique_ptr<AST> FOR_STMT();
        std::unique_ptr<AST> BREAK_STMT();
        std::unique_ptr<AST> CONTINUE_STMT();
        std::unique_ptr<ASTReturn> RETURN_STMT();
        std::unique_ptr<AST> EXPR_STMT();
        std::unique_ptr<ASTType> TYPE(bool canBeVoid = false);
        std::unique_ptr<ASTType> TYPE_FUN_RET();
        std::unique_ptr<ASTStructDecl> STRUCT_DECL();
        std::unique_ptr<ASTFunPtrDecl> FUNPTR_DECL();
        std::unique_ptr<AST> EXPR_OR_VAR_DECL();
        std::unique_ptr<ASTVarDecl> VAR_DECL();
        std::unique_ptr<AST> VAR_DECLS();
        std::unique_ptr<AST> EXPR();
        std::unique_ptr<AST> EXPRS();
        std::unique_ptr<AST> E9();
        std::unique_ptr<AST> E8();
        std::unique_ptr<AST> E7();
        std::unique_ptr<AST> E6();
        std::unique_ptr<AST> E5();
        std::unique_ptr<AST> E4();
        std::unique_ptr<AST> E3();
        std::unique_ptr<AST> E2();
        std::unique_ptr<AST> E1();
        std::unique_ptr<AST> E_UNARY_PRE();
        std::unique_ptr<AST> E_CALL_INDEX_MEMBER_POST();
        std::unique_ptr<AST> F();
        std::unique_ptr<ASTIdentifier> IDENT();

    private:
        std::vector<Token> tokens_;
        size_t i_ = 0;

    }; // tiny::Parser

} // namespace tiny
