
#pragma once

#include "il.h"

namespace tiny {

    /** A very simple interpreter of our intermediate language. 

        Such code is of course not part of a real compiler, but it's quite helpful for us to validate the translated IL and any transformations on it by running it and comparing to the result of the program. 
     */
    class ILInterpreter {
    public:
        static uint64_t run(Program const & p) {
            ILInterpreter i{p};
            BasicBlock const * b = p.globals();
            i.locals_ = & i.globals_; // for the execution of the global context
            b = i.runBasicBlock(b);
            ASSERT(b == nullptr && "We only support single globals basic block");
            Function const * f = p.getFunction(Symbol{"main"});
            ASSERT(f != nullptr);
            std::vector<Reg> args;
            Reg result = i.runFunction(f, args);
            ASSERT(result.ins->type == RegType::Int);
            return result.iVal;
        }

    private:

        ILInterpreter(Program const & p):p_{p} {}

        /** Register value. 
         */
        struct Reg {
            Instruction const * ins;
            union {
                int64_t iVal;
                double fVal;
            };

            Reg(): ins{nullptr}, iVal{0} {} 

            Reg(int64_t value, Instruction const * ins):
                ins{ins},
                iVal{value} {
                ASSERT(ins->type == RegType::Int);
            }

            Reg(double value, Instruction const * ins):
                ins{ins},
                fVal{value} {
                ASSERT(ins->type == RegType::Float);
            }

            size_t memSize() const {
                switch (ins->type) {
                    case RegType::Int:
                        return sizeof(int64_t);
                    case RegType::Float:
                        return sizeof(double);
                    case RegType::Void:
                        return 0;
                    default:
                        UNREACHABLE;
                }
            }
        }; // ILInterpreter::Reg

        /** Memory abstraction for the interpreter. We keep two areas - stack (and globals) and heap.
         */
        struct Memory {
            std::vector<uint8_t> stack;
            std::vector<uint8_t> heap;

            size_t const heapStart = 0x100000; 

            int64_t alloc(int64_t numBytes) {
                ASSERT(numBytes > 0 && "Negative and zero allocations are not allowed");
                size_t addr = stack.size();
                stack.resize(stack.size() + static_cast<size_t>(numBytes));
                ASSERT(stack.size() < heapStart && "Stack overflow!"); 
                return static_cast<int64_t>(addr);
            }

            int64_t readInt(int64_t address) {
                ASSERT(address >= 0);
                size_t addr = static_cast<size_t>(address);
                std::vector<uint8_t> & m = (addr > heapStart) ? heap : stack;
                ASSERT(addr + sizeof(int64_t) <= m.size() && "Say segfault!");
                return *reinterpret_cast<int64_t*>(m.data() + address);    
            } 

            double readFloat(int64_t address) {
                ASSERT(address >= 0);
                size_t addr = static_cast<size_t>(address);
                std::vector<uint8_t> & m = (addr > heapStart) ? heap : stack;
                ASSERT(addr + sizeof(double) <= m.size() && "Say segfault!");
                return *reinterpret_cast<double*>(m.data() + address);    
            }

            void write(int64_t address, Reg value) {
                ASSERT(address >= 0);
                size_t addr = static_cast<size_t>(address);
                std::vector<uint8_t> & m = (addr > heapStart) ? heap : stack;
                if (addr > heapStart) 
                    addr -= heapStart;
                if (addr + value.memSize() > m.size())
                    m.resize(addr + value.memSize());
                switch (value.ins->type) {
                    case RegType::Int:
                        *reinterpret_cast<int64_t*>(m.data() + address) = value.iVal;
                        break;
                    case RegType::Float:
                        *reinterpret_cast<double*>(m.data() + address) = value.fVal;
                        break;
                    case RegType::Void:
                        ASSERT(false && "Cannot save void value");
                }
            }
        }; // ILInterpreter::Memory
        
        Reg runFunction(Function const * f, std::vector<Reg> const & args) {
            auto oldLocals = locals_;
            std::unordered_map<Instruction const *, Reg> locals;
            locals_ = & locals;
            ASSERT(args.size() == f->numArgs() && "Function call argument mismatch");
            for (size_t i = 0, e = f->numArgs(); i != e; ++i)
                set(f->getArg(i), args[i]);
            BasicBlock const * bb = f->start();
            while (bb != nullptr) {
                ASSERT(bb->terminated());
                bb = runBasicBlock(bb);
            }
            locals_ = oldLocals;
            return retVal_;
        }

        /** Runs the given basic block and returns the basic block to be executed next, or nullptr if the function should return.
         */
        BasicBlock const * runBasicBlock(BasicBlock const * bb) {
            BasicBlock * next = nullptr;
            bool terminated = false;
            for (size_t i = 0, e = bb->size(); i != e; ++i) {
                Instruction const * ins = (*bb)[i];
                switch (ins->opcode) {
                    case Opcode::LDI: {
                        set(ins, IMMI(ins)->value);
                        break;
                    }
                    case Opcode::LDF: {
                        set(ins, IMMF(ins)->value);
                        break; 
                    }
                    case Opcode::LD: {
                        Reg addr = get(REG(ins)->reg);
                        ASSERT(addr.ins->type == RegType::Int && "Address must be int");
                        switch (ins->type) {
                            case RegType::Int:
                                set(ins, mem_.readInt(addr.iVal));
                                break;
                            case RegType::Float:
                                set(ins, mem_.readFloat(addr.iVal));
                                break;
                            case RegType::Void:
                                ASSERT(false && "cannot load to void register");
                                break;
                        }
                        break;
                    }
                    case Opcode::ST: {
                        auto st = REG_REG(ins);
                        ASSERT(st->reg1->type == RegType::Int);
                        mem_.write(get(st->reg1).iVal, get(st->reg2));
                        break;
                    }
                    case Opcode::ALLOCA:
                    case Opcode::ALLOCG: {
                        int64_t  size = IMMI(ins)->value;
                        set(ins, mem_.alloc(size));
                        break;
                    }
                    case Opcode::ADD: {
                        NOT_IMPLEMENTED;
                    }
                    case Opcode::SUB: {
                        NOT_IMPLEMENTED;
                    }
                    case Opcode::MUL: {
                        NOT_IMPLEMENTED;
                    }
                    case Opcode::DIV: {
                        NOT_IMPLEMENTED;
                    }
                    /** The FUN instruction is just a placeholder for function declaration. No need to do anything when interpreting it as all functions are available in the symbols list.
                     */
                    case Opcode::FUN: {
                        break;
                    }
                    /** Call is simple from the interpreter's perspective. We need to start new context for the function and initialize the callee's argument registers with caller's argument registers. We then call the function itself. */
                    case Opcode::CALL: {
                        auto call = REG_REGS(ins);
                        auto f = p_.getFunction(IMMS(call->reg)->value);
                        ASSERT(f != nullptr && "We must know the function we wish to call"); 
                        ASSERT(call->regs.size() == f->numArgs());
                        std::vector<Reg> args;
                        for (Instruction * arg : call->regs)
                            args.push_back(get(arg));
                        // call the function, augment the return value with the CALL instruction as its own for bookkeeping
                        Reg result = runFunction(f, args);
                        result.ins = ins;
                        set(ins, result);
                        break;
                    }
                    case Opcode::ARG: {
                        //set(ins, (*args_)[IMMI(ins)->value]);
                        break;
                    }
                    case Opcode::RET: {
                        ASSERT(!terminated);
                        terminated = true;
                        break;
                    }
                    case Opcode::RETR: {
                        ASSERT(!terminated);
                        terminated = true;
                        retVal_ = get(TERMINATOR_REG(ins)->reg);
                        break;
                    }
                    case Opcode::JMP: {
                        ASSERT(!terminated);
                        terminated = true;
                        next = TERMINATOR_B(ins)->target;
                        break;
                    }
                    case Opcode::BR: {
                        ASSERT(!terminated);
                        terminated = true;
                        auto br = TERMINATOR_REG_BB(ins);
                        Reg cond = get(br->reg);
                        ASSERT(cond.ins->type == RegType::Int);
                        next = (cond.iVal == 0) ? br->target2 : br->target1;
                        break;
                    }
                    default:
                        NOT_IMPLEMENTED;
                }
            }



            return next;
        }

        static Instruction::ImmI const * IMMI(Instruction const * ins) {
            return dynamic_cast<Instruction::ImmI const *>(ins);
        }

        static Instruction::ImmF const * IMMF(Instruction const * ins) {
            return dynamic_cast<Instruction::ImmF const *>(ins);
        }

        static Instruction::ImmS const * IMMS(Instruction const * ins) {
            return dynamic_cast<Instruction::ImmS const *>(ins);
        }

        static Instruction::Reg const * REG(Instruction const * ins) {
            return dynamic_cast<Instruction::Reg const *>(ins);
        }

        static Instruction::RegReg const * REG_REG(Instruction const * ins) {
            return dynamic_cast<Instruction::RegReg const *>(ins);
        }

        static Instruction::RegRegs const * REG_REGS(Instruction const * ins) {
            return dynamic_cast<Instruction::RegRegs const *>(ins);
        }

        static Instruction::TerminatorReg const * TERMINATOR_REG(Instruction const * ins) {
            return dynamic_cast<Instruction::TerminatorReg const *>(ins);
        }
        static Instruction::TerminatorB const * TERMINATOR_B(Instruction const * ins) {
            return dynamic_cast<Instruction::TerminatorB const *>(ins);
        }
        static Instruction::TerminatorRegBB const * TERMINATOR_REG_BB(Instruction const * ins) {
            return dynamic_cast<Instruction::TerminatorRegBB const *>(ins);
        }

        void set(Instruction const * ins, int64_t value) {
            ASSERT(ins->type == RegType::Int);
            (*locals_)[ins] = Reg{value, ins};
        }

        void set(Instruction const * ins, double value) {
            ASSERT(ins->type == RegType::Float);
            (*locals_)[ins] = Reg{value, ins};
        }

        void set(Instruction const * ins, Reg value) {
            switch (value.ins->type) {
                case RegType::Int:
                    set(ins, value.iVal);
                    break;
                case RegType::Float:
                    set(ins, value.fVal);
                    break;
                default:
                    break; //we do not store voids
            }
        }


        Reg get(Instruction const * ins) {
            ASSERT(ins->type != RegType::Void);
            auto i = locals_->find(ins);
            if (i == locals_->end()) {
                i = globals_.find(ins);
                ASSERT(i != globals_.end());
            }
            return i->second;
        }

        Program const & p_;

        Memory mem_;
        // global registers
        std::unordered_map<Instruction const *,Reg> globals_;
        // and local registers, set for each function
        std::unordered_map<Instruction const *, Reg> * locals_;
        // return value from a function
        Reg retVal_;


    }; // tiny::ILInterpreter


} // namespace tiny

