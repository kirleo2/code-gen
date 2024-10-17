#include "il.h"

namespace tiny {

    void Instruction::TerminatorB::print(colors::ColorPrinter & p) const {
        using namespace colors;
        Instruction::print(p);
        p << " " << (*target);
    }

    void Instruction::TerminatorRegBB::print(colors::ColorPrinter & p) const {
        using namespace colors;
        Instruction::print(p);
        p << " " << (*reg) << SYMBOL("? ") << (*target1) << SYMBOL(" : ") << (*target2);
    }

}