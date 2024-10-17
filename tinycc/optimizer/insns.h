
#ifndef INS
#error "it feels wrong to include insns.h without definiting the INS macro"
#endif

/** Loads integer or float immediate values. 
  
 */
INS(LDI, ImmI)
INS(LDF, ImmF)

/** Loads int or float value stored at the address stored in the register. 
 */
INS(LD, Reg)

/** Stores the value in second register to memory address at first register.
 */
INS(ST, RegReg)

/** Allocates new variable either on heap (ALLOCG), or stack (ALLOCA) and returns its address. The immediate argument is the number of bytes to be allocated.
 */
INS(ALLOCA, ImmI)
INS(ALLOCG, ImmI)

/** memcpy
 */
INS(COPY, RegRegImmI)

/** Get Element pointer. 
 */
INS(GEP, RegRegImmI)

/** Binary operators on numbers. 
 
    They work with both float and double registers. 
*/
INS(ADD, RegReg)
INS(SUB, RegReg)
INS(MUL, RegReg)
INS(DIV, RegReg)
INS(MOD, RegReg)
INS(SHR, RegReg)
INS(SHL, RegReg)

/** Bitwise operations. 
 
    They can be used as their boolean counterparts after converting to boolean (0 or 1 values) as well. 
*/
INS(AND, RegReg)
INS(OR, RegReg)
INS(XOR, RegReg)
INS(NEG, RegReg)

/** Comparison operators. 
  
 */
INS(LT, RegReg)
INS(LTE, RegReg)
INS(GT, RegReg)
INS(GTE, RegReg)
INS(EQ, RegReg)

INS(FUN, ImmS)
INS(CALL, RegRegs)
INS(ARG, ImmI)

INS(RET, Terminator)
INS(RETR, TerminatorReg)
INS(JMP, TerminatorB)
INS(BR, TerminatorRegBB)

#undef INS
