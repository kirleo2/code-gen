#pragma once
#include <cstdint>
#include <unordered_map>

namespace tiny {


    template<typename T>
    class State {
    public:

        T & get(Instruction * reg) {
            return state_[reg]; // create bottom if does not exist yet
        }

        void set(Instruction * reg, T val) {
            state_[reg] = val;
        }

        bool mergeWith(State<T> const & other) {
            // merge all held values, including those onl in the orher state
            // TODO 
            NOT_IMPLEMENTED;
        }

    private:
        std::unordered_map<Instruction *, T> state_;

    }; // tiny::State



    template<typename T>
    class ForwardAnalysis {
    public:

        void analyze(BasicBlock * start, State<T> initialState) {
            q_.push_back(start, initialState);
            while (!q_.empty()) {
                // get next basic block to analyze
                BasicBlock * b = q_.back();
                q_.pop_back();
                State<T> state = inputStates_[b];
                // process all instructions
                for (size_t i = 0, e = b->size(); i != e; ++i)
                    processInstruction(b[i], state);
                // add next states
                // TODO (depends on your API for terminators)
                // pseudocode: for all next blocks: analyzeBB(next, state);
            }
        }
       

    protected:

        void analyzeBB(BasicBlock * b, State<T> const & inputState) {
            if (inputStates_[b].mergeWith(inputState))
                q_.push_back(b);
        }

        virtual void processInstruction(Instruction * ins, State<T> & state) = 0;

        std::vector<BasicBlock *> q_;
        std::unordered_map<BasicBlock *, State<T>> inputStates_;
    }; // tiny::ForwardAnalysis


    struct SimpleCPValue {
        enum class Kind {
            Bottom, 
            Constant, 
            NonZero,
            Top, 
        }; 
        Kind kind;
        int64_t value = 0;

        SimpleCPValue(): kind{Kind::Bottom} {}

        /** Merges the value with other. Returns true if there was change, false otherwise.
         */
        bool mergeWith(SimpleCPValue const & other) {
            // if we are bottom, take the other value
            if (kind == Kind::Bottom) {
                kind = other.kind;
                value = other.value;
                return kind != Kind::Bottom;
            };
            // if we are top, we can't change
            if (kind == Kind::Top)
                return false;
            // if we are the same, nothing changes
            if (*this == other)
                return false;
            // if the other one is top, we change to top
            if (other.kind == Kind::Top) {
                kind = Kind::Top;
                return true;
            }
            // if the other one is bottom, then we don't change
            if (other.kind == Kind::Bottom)
                return false;
            // TODO the rest
            NOT_IMPLEMENTED;
        }

        bool operator == (SimpleCPValue const & other) const {
            return kind == other.kind && (kind != Kind::Constant || (value == other.value));
        }
    }; // tiny::SimpleCPValue

    class CPAnalysis : public ForwardAnalysis<SimpleCPValue> {
    public:

        
    protected:

        void processInstruction(Instruction * ins, State<SimpleCPValue> & state) override {
            switch (ins->opcode) {
                case Opcode::ADD:
                    // TODO
                    break;
            }
            NOT_IMPLEMENTED;
        }

    }; // CPAnalysis

} // namespace tiny

