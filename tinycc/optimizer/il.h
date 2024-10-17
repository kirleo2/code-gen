#pragma once


#include <unordered_map>
#include <memory>

#include "common/colors.h"
#include "frontend/ast.h"

namespace tiny {

    class Function;
    class BasicBlock;
    class Instruction;

    enum class Opcode {
#define INS(OPCODE, ...) OPCODE,
#include "insns.h"
    }; // tiny::il::Opcode

    enum class RegType {
        Int, 
        Float, 
        Void,
    }; // tiny::il::RegType

    inline colors::ColorPrinter & operator << (colors::ColorPrinter & p, RegType type) {
        using namespace colors;
        switch (type) {
            case RegType::Int : p << TYPE("int"); break;
            case RegType::Float : p << TYPE("float"); break;
            case RegType::Void : p << TYPE("void"); break;
            default:
                UNREACHABLE;
                break;
        }
        return p;
    }

    inline colors::ColorPrinter & operator << (colors::ColorPrinter & p, Opcode opcode) {
        using namespace colors;
        switch (opcode) {
#define INS(OPCODE, ...) case Opcode::OPCODE: p << KEYWORD(#OPCODE); break;
#include "insns.h"
            default:
                UNREACHABLE;
                break;
        }
        return p;
    }

    /** Basic instruction. 
     */
    class Instruction {
    public:

        class ImmI;
        class ImmF;
        class ImmS;
        class Reg;
        class RegReg;
        class RegRegImmI;
        class RegRegs;
        class Terminator;
        class TerminatorB;
        class TerminatorReg;
        class TerminatorRegBB;

        virtual ~Instruction() = default;

        Opcode const opcode;
        RegType const type;
        AST const * const ast;
        std::string const name;
    
    protected:

        friend class BasicBlock;
        friend class Function;

        Instruction(Opcode opcode, RegType type, AST const * ast, std::string const & name):
            opcode{opcode},
            type{type},
            ast{ast},
            name{makeUniqueName(name)} {
        }

        Instruction(Opcode opcode, RegType type, AST const * ast = nullptr):
            opcode{opcode},
            type{type},
            ast{ast},
            name{makeUniqueName()} {
        }

        virtual void print(colors::ColorPrinter & p) const {
            using namespace colors;
            if (type != RegType::Void) {
                p << IDENT(name) << SYMBOL(": ") << type << SYMBOL(" = ");
            }
            p << opcode;
        }

        friend colors::ColorPrinter & operator << (colors::ColorPrinter & p, Instruction const & ins) {
            using namespace colors;
            p << IDENT(ins.name) << SYMBOL(": ") << ins.type;
            return p;
        }

        static std::string makeUniqueName() {
            return STR("r" << nextUniqueId());
        }

        static std::string makeUniqueName(std::string const & from) {
            return STR(from << nextUniqueId());
        }

        static size_t nextUniqueId() {
            static size_t i = 0;
            return i++;
        }
    }; // tiny::il::Instruction

    class Instruction::ImmI : public Instruction {
    public:
        int64_t value;

        ImmI(Opcode opcode, RegType type, int64_t value, AST const * ast = nullptr):
            Instruction{opcode, type, ast},
            value{value} {
        }

        ImmI(Opcode opcode, RegType type, int64_t value, AST const * ast, std::string const & name):
            Instruction{opcode, type, ast, name},
            value{value} {
        }

        ImmI(Opcode opcode, RegType type, int64_t value, std::string const & name):
            Instruction{opcode, type, nullptr, name},
            value{value} {
        }

    protected:

        void print(colors::ColorPrinter & p) const override {
            using namespace colors;
            Instruction::print(p);
            p << " " << value;
        }

    }; // tiny::il::Instruction::ImmI

    class Instruction::ImmF : public Instruction {
    public:
        double value;

        ImmF(Opcode opcode, RegType type, double value, AST const * ast = nullptr):
            Instruction{opcode, type, ast},
            value{value} {
        }

        ImmF(Opcode opcode, RegType type, double value, AST const * ast, std::string const & name):
            Instruction{opcode, type, ast, name},
            value{value} {
        }

        ImmF(Opcode opcode, RegType type, double value, std::string const & name):
            Instruction{opcode, type, nullptr, name},
            value{value} {
        }

    protected:

        void print(colors::ColorPrinter & p) const override {
            using namespace colors;
            Instruction::print(p);
            p << " " << value;
        }


    }; // tiny::il::Instruction::ImmF

    class Instruction::ImmS : public Instruction {
    public:
        Symbol value;

        ImmS(Opcode opcode, Symbol value, AST const * ast = nullptr):
            Instruction{opcode, RegType::Int, ast},
            value{value} {
        }

        ImmS(Opcode opcode, Symbol value, AST const * ast, std::string const & name):
            Instruction{opcode, RegType::Int, ast, name},
            value{value} {
        }

        ImmS(Opcode opcode, Symbol value, std::string const & name):
            Instruction{opcode, RegType::Int, nullptr, name},
            value{value} {
        }

    protected:

        void print(colors::ColorPrinter & p) const override {
            using namespace colors;
            Instruction::print(p);
            p << " " << value;
        }

    }; // tiny::il::Instruction::ImmS


    class Instruction::Reg : public Instruction {
    public:
        Instruction * reg;

        Reg(Opcode opcode, RegType type, Instruction * reg, AST const * ast = nullptr):
            Instruction{opcode, type, ast},
            reg{reg} {
            ASSERT(reg != nullptr);
        }

        Reg(Opcode opcode, RegType type, Instruction * reg, AST const * ast, std::string const & name):
            Instruction{opcode, type, ast, name},
            reg{reg} {
            ASSERT(reg != nullptr);
        }

        Reg(Opcode opcode, RegType type, Instruction * reg, std::string const & name):
            Instruction{opcode, type, nullptr, name},
            reg{reg} {
            ASSERT(reg != nullptr);
        }

    protected:

        void print(colors::ColorPrinter & p) const override {
            using namespace colors;
            Instruction::print(p);
            p << " " << (*reg);
        }

    }; // tiny::il::Instruction::Reg

    class Instruction::RegReg : public Instruction {
    public:
        Instruction * reg1;
        Instruction * reg2;

        RegReg(Opcode opcode, RegType type, Instruction * reg1, Instruction * reg2, AST const * ast = nullptr):
            Instruction{opcode, type, ast},
            reg1{reg1},
            reg2{reg2} {
        }

        RegReg(Opcode opcode, RegType type, Instruction * reg1, Instruction * reg2, AST const * ast, std::string const & name):
            Instruction{opcode, type, ast, name},
            reg1{reg1},
            reg2{reg2} {
        }

        RegReg(Opcode opcode, RegType type, Instruction * reg1, Instruction * reg2, std::string const & name):
            Instruction{opcode, type, nullptr, name},
            reg1{reg1},
            reg2{reg2} {
        }

        RegReg(Opcode opcode, Instruction * reg1, Instruction * reg2, AST const * ast = nullptr):
            RegReg{opcode, RegType::Void, reg1, reg2, ast} {
        }

        RegReg(Opcode opcode, Instruction * reg1, Instruction * reg2, AST const * ast, std::string const & name):
            RegReg{opcode, RegType::Void, reg1, reg2, ast, name} {
        }

        RegReg(Opcode opcode, Instruction * reg1, Instruction * reg2, std::string const & name):
            RegReg{opcode, RegType::Void, reg1, reg2, nullptr, name} {
        }


    protected:

        void print(colors::ColorPrinter & p) const override {
            using namespace colors;
            Instruction::print(p);
            p << " " << (*reg1) << SYMBOL(", ") << (*reg2);
        }


    }; // tiny::il::Instruction::RegReg

    class Instruction::RegRegImmI : public Instruction {
    public:
        Instruction * reg1;
        Instruction * reg2;
        int64_t value;

        RegRegImmI(Opcode opcode, RegType type, Instruction * reg1, Instruction * reg2, int64_t value, AST const * ast = nullptr):
            Instruction{opcode, type, ast},
            reg1{reg1},
            reg2{reg2},
            value{value} {
        }

        RegRegImmI(Opcode opcode, RegType type, Instruction * reg1, Instruction * reg2, int64_t value, AST const * ast, std::string const & name):
            Instruction{opcode, type, ast, name},
            reg1{reg1},
            reg2{reg2},
            value{value} {
        }

        RegRegImmI(Opcode opcode, RegType type, Instruction * reg1, Instruction * reg2, int64_t value, std::string const & name):
            Instruction{opcode, type, nullptr, name},
            reg1{reg1},
            reg2{reg2},
            value{value} {
        }

        RegRegImmI(Opcode opcode, Instruction * reg1, Instruction * reg2, int64_t value, AST const * ast = nullptr):
            RegRegImmI{opcode, RegType::Void, reg1, reg2, value, ast} {
        }

        RegRegImmI(Opcode opcode, Instruction * reg1, Instruction * reg2, int64_t value, AST const * ast, std::string const & name):
            RegRegImmI{opcode, RegType::Void, reg1, reg2, value, ast, name} {
        }

        RegRegImmI(Opcode opcode, Instruction * reg1, Instruction * reg2, int64_t value, std::string const & name):
            RegRegImmI{opcode, RegType::Void, reg1, reg2, value, nullptr, name} {
        }


    protected:

        void print(colors::ColorPrinter & p) const override {
            using namespace colors;
            Instruction::print(p);
            p << " " << (*reg1) << SYMBOL(", ") << (*reg2);
        }


    }; // tiny::il::Instruction::RegRegImmI



    class Instruction::RegRegs : public Instruction {
    public:
        Instruction * reg;
        std::vector<Instruction *> regs;

        RegRegs(Opcode opcode, RegType type, Instruction * reg, std::vector<Instruction *> & regs, AST const * ast = nullptr):
            Instruction{opcode, type, ast},
            reg{reg},
            regs{regs} {
        }

        RegRegs(Opcode opcode, RegType type, Instruction * reg, std::vector<Instruction *> & regs, AST const * ast, std::string const & name):
            Instruction{opcode, type, ast, name},
            reg{reg},
            regs{regs} {
        }

        RegRegs(Opcode opcode, RegType type, Instruction * reg, std::vector<Instruction *> & regs, std::string const & name):
            Instruction{opcode, type, nullptr, name},
            reg{reg},
            regs{regs} {
        }

    protected:

        void print(colors::ColorPrinter & p) const override {
            using namespace colors;
            Instruction::print(p);
            p << " " << (*reg);
            for (Instruction * r : regs) 
                p << *r << SYMBOL(", ");
            p << SYMBOL(")");
        }

    }; // tiny::il::Instruction::RegRegs

    class Instruction::Terminator : public Instruction {
    public:
        Terminator(Opcode opcode, AST const * ast, std::string const & name):
            Instruction{opcode, RegType::Void, ast, name} {
        }

        Terminator(Opcode opcode, AST const * ast = nullptr):
            Instruction{opcode, RegType::Void, ast} {
        }
        Terminator(Opcode opcode, std::string const & name):
            Instruction{opcode, RegType::Void, nullptr, name} {
        }
    }; // tiny::il::Instruction::Terminator

    class Instruction::TerminatorB : public Instruction::Terminator {
    public:
        BasicBlock * target;

        TerminatorB(Opcode opcode, BasicBlock * target, AST const * ast = nullptr):
            Terminator{opcode, ast},
            target{target} {
        }

        TerminatorB(Opcode opcode, BasicBlock * target, AST const * ast, std::string const & name):
            Terminator{opcode, ast, name},
            target{target} {
        }

        TerminatorB(Opcode opcode, BasicBlock * target, std::string const & name):
            Terminator{opcode, nullptr, name},
            target{target} {
        }

    protected:

        void print(colors::ColorPrinter & p) const override;

    }; // tiny::il::Instruction::TerminatorB

    class Instruction::TerminatorReg : public Instruction::Terminator {
    public:
        Instruction * reg;

        TerminatorReg(Opcode opcode, Instruction * reg, AST const * ast = nullptr):
            Terminator{opcode, ast},
            reg{reg} {
        }

        TerminatorReg(Opcode opcode, Instruction * reg, AST const * ast, std::string const & name):
            Terminator{opcode, ast, name},
            reg{reg} {
        }

        TerminatorReg(Opcode opcode, Instruction * reg, std::string const & name):
            Terminator{opcode, nullptr, name},
            reg{reg} {
        }

    protected:

        void print(colors::ColorPrinter & p) const override {
            using namespace colors;
            Instruction::print(p);
            p << " " << (*reg);
        }

    };// tiny::il::Instruction::TerminatorReg

    class Instruction::TerminatorRegBB : public Instruction::Terminator {
    public:
        Instruction * reg;
        BasicBlock * target1;
        BasicBlock * target2;

        TerminatorRegBB(Opcode opcode, Instruction * reg, BasicBlock * target1, BasicBlock * target2, AST const * ast = nullptr):
            Terminator{opcode, ast},
            reg{reg},
            target1{target1},
            target2{target2} {
        }

        TerminatorRegBB(Opcode opcode, Instruction * reg, BasicBlock * target1, BasicBlock * target2, AST const * ast, std::string const & name):
            Terminator{opcode, ast, name},
            reg{reg},
            target1{target1},
            target2{target2} {
        }

        TerminatorRegBB(Opcode opcode,Instruction * reg, BasicBlock * target1, BasicBlock * target2, std::string const & name):
            Terminator{opcode, nullptr, name},
            reg{reg},
            target1{target1},
            target2{target2} {
        }

    protected:

        void print(colors::ColorPrinter & p) const override;

    }; // tiny::il::Instruction::RegBB



#define INS(NAME, ENCODING) \
    template<typename... Args> \
    Instruction::ENCODING * NAME(Args... args) { return new Instruction::ENCODING{Opcode::NAME, args...}; }
#include "insns.h"

    /** Basic block. 
     */
    class BasicBlock {
    public:

        std::string const name;

        BasicBlock():
            name{makeUniqueName()} {
        }

        BasicBlock(std::string const & name): 
            name{makeUniqueName(name)} {
        }
        
        bool terminated() const {
            if (insns_.empty())
                return false;
            return dynamic_cast<Instruction::Terminator*>(insns_.back().get()) != nullptr;
        }

        /** Appends the instruction to the given basic block. 
         */
        Instruction * append(Instruction * ins) {
            insns_.push_back(std::unique_ptr<Instruction>{ins});
            return ins;
        }

        size_t size() const { return insns_.size(); }

        Instruction const * operator[](size_t i) const { return insns_[i].get(); }



    
    private:

        friend class Function; 
        friend class Program;

        void print(colors::ColorPrinter & p) const {
            using namespace colors;
            p << IDENT(name) << SYMBOL(":") << INDENT;
            for (auto & i : insns_) {
                p << NEWLINE;
                i->print(p);
            }
            p << DEDENT;
        }


        static std::string makeUniqueName() {
            return STR("bb" << nextUniqueId());
        }

        static std::string makeUniqueName(std::string const & from) {
            return STR(from << nextUniqueId());
        }

        static size_t nextUniqueId() {
            static size_t i = 0;
            return i++;
        }

        std::vector<std::unique_ptr<Instruction>> insns_;

    }; 

    inline colors::ColorPrinter & operator << (colors::ColorPrinter & p, BasicBlock const & b) {
        using namespace colors;
        p << IDENT(b.name);
        return p;
    }

    /** Function. 
    */
    class Function {
    public:
        BasicBlock * addBasicBlock() {
            std::unique_ptr<BasicBlock> bb{new BasicBlock{}};
            bbs_.push_back(std::move(bb));
            return bbs_.back().get();
        }

        BasicBlock * addBasicBlock(std::string const & name) {
            std::unique_ptr<BasicBlock> bb{new BasicBlock{name}};
            bbs_.push_back(std::move(bb));
            return bbs_.back().get();
        }

        Instruction * addArg(Instruction * arg) {
            std::unique_ptr<Instruction> a{arg};
            args_.push_back(std::move(a));
            return arg;
        }

        size_t numArgs() const { return args_.size(); }

        Instruction const * getArg(size_t i) const { return args_[i].get(); }

        void print(colors::ColorPrinter & p) const {
            using namespace colors;
            if (! args_.empty()) {
                p << NEWLINE << COMMENT("; arguments ") << INDENT;
                for (auto & arg : args_) {
                    p << NEWLINE;
                    arg->print(p);
                }
                p << DEDENT;
            }
            p << NEWLINE << COMMENT("; number of basic blocks: ") << bbs_.size();
            for (auto & bb : bbs_) {
                p << NEWLINE;
                bb->print(p);
            }
        }

        BasicBlock * start() const { return bbs_[0].get(); }
    private:
        std::vector<std::unique_ptr<Instruction>> args_;
        std::vector<std::unique_ptr<BasicBlock>> bbs_;
    }; 

    /** Program 
     */
    class Program {
    public:

        Function * addFunction(Symbol name){
            Function * f = new Function{};
            functions_.insert(std::make_pair(name, f));
            return f;
        }

        BasicBlock const * globals() const {
            return & globals_;
        }

        BasicBlock * globals() {
            return & globals_;
        }

        Function const * getFunction(Symbol name) const {
            auto i = functions_.find(name);
            return (i == functions_.end()) ? nullptr : i->second;
        }

        void print(colors::ColorPrinter & p) const {
            using namespace colors;
            p << COMMENT("; globals") << NEWLINE << INDENT;
            globals_.print(p);
            p << DEDENT << NEWLINE << COMMENT("; number of functions: ") << functions_.size() << NEWLINE;
            for (auto f : functions_) {
                p << IDENT("function_") << f.first << SYMBOL(":") << INDENT;
                f.second->print(p);
                p << DEDENT << NEWLINE;
            }
        }

    private:

        BasicBlock globals_;
        std::unordered_map<Symbol, Function *> functions_;
    }; 



} // namespace tiny

