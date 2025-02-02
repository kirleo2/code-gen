
#ifndef TINYCC_INSTRUCTIONS_HPP
#define TINYCC_INSTRUCTIONS_HPP

#include <iostream>


#define MOV "MOV"
#define LEA "LEA"

#define ADD "ADD"
#define SUB "SUB"
#define INC "INC"
#define MUL "MUL"
#define DIV "DIV"
#define IMUL "IMUL"
#define IDIV "IDIV"

#define FADD "FADD"
#define FSUB "FSUB"
#define FMUL "FMUL"
#define FDIV "FDIV"

#define AND "AND"
#define OR "OR"
#define XOR "XOR"
#define LSH "LSH"
#define RSH "RSH"

#define CMP "CMP"
#define FCMP "FCMP"

#define JMP "JMP"
#define LOOP "LOOP"
#define JZ "JZ"
#define JNZ "JNZ"
#define JE "JE"
#define JNE "JNE"
#define JG "JG"
#define JGE "JGE"
#define JL "JL"
#define JLE "JLE"
#define JA "JA"
#define JAE "JAE"
#define JB "JB"
#define JBE "JBE"
#define JO "JO"
#define JNO "JNO"
#define JS "JS"
#define JNS "JNS"

#define CALL "CALL"
#define RET "RET"

#define PUSH "PUSH"
#define FPUSH "FPUSH"
#define POP "POP"
#define FPOP "FPOP"

#define PUTCHAR "PUTCHAR"
#define PUTNUM "PUTNUM"

#define GETCHAR "GETCHAR"
#define EXT "EXT"
#define NRW "NRW"
#define DBG "DBG"
#define BREAK "BREAK"
#define BKPT "BKPT"
#define HALT "HALT"
#define NOP "NOP"

#define NOREG 0
#define SP 1
#define BP 2
#define PC 3
#define FLAGS 4
#define R0 5 // return value
#define R1 6 // temporary

typedef size_t Register;
class Operand {

public:
  Operand(Register r, int64_t offset): _register1(r), _register2(NOREG), _offset(offset), _multiplier(1), _isMemoryAccess(false) {}
  Operand(Register r): _register1(r), _register2(NOREG), _offset(0), _multiplier(1), _isMemoryAccess(false) {
    assert(r != FLAGS);
  }

  Operand(Register r1, Register r2,  int64_t offset): _register1(r1), _register2(r2), _offset(offset), _multiplier(1), _isMemoryAccess(false) {}
  Operand(Register r1, Register r2): _register1(r1), _register2(r2), _offset(0), _multiplier(1), _isMemoryAccess(false) {
    assert(r1 != FLAGS && r2 != FLAGS);
  }

  Operand & setMemoryAccess() {
    _isMemoryAccess = true;
    return *this;
  }

  Operand & setOffset(int64_t offset) {
    _offset = offset;
    return *this;
  }
  Operand & increaseOffset(int64_t offset) {
    _offset += offset;
    return *this;
  }

  Operand & setMultiplier(int64_t multiplier) {
    _multiplier = multiplier;
    return *this;
  }

  Operand & setRegister1(Register reg) {
    _register1 = reg;
    return *this;
  }

  Operand & setRegister2(Register reg) {
    _register2 = reg;
    return *this;
  }

  int64_t getOffset() {
    return _offset;
  }

  static void printRegister(Register reg, std::ostream& os) {
    switch (reg) {
      case SP:
        os << "SP";
        break;
      case FLAGS:
        assert(0);
        break;
      case BP:
        os << "BP";
        break;
      case PC:
        os << "PC";
        break;
      case R0:
        os << "R0";
        break;
      case R1:
        os << "R1";
        break;
      default:
        assert(reg > R1);
        os << "R" << reg - R1 + 1;
        break;
    }
  }

  friend std::ostream& operator<<(std::ostream& os, const Operand & op) {
    if (op._isMemoryAccess) os << "[";
    if (op._register1 != NOREG) {
      if (op._multiplier != 1) {
        os << op._multiplier << "*";
      }
      Operand::printRegister(op._register1, os);
      // || (op._register2 != NOREG) is a silly workaround for broken t86
      if (op._offset != 0 || (op._register2 != NOREG)) {
        os << (op._offset >= 0 ? " + " : " - ") << abs(op._offset);
      }
      if (op._register2 != NOREG) {
        os << " + ";
        Operand::printRegister(op._register2, os);
      }
    } else if (op._register2 != NOREG) {
      Operand::printRegister(op._register2, os);
      if (op._offset != 0) {
        os << (op._offset > 0 ? " + " : " - ") << abs(op._offset);
      }
    } else {
      assert(op._offset >= 0);
      os << op._offset;
    }

    if (op._isMemoryAccess) os << "]";
    return os;
  }

public:
  Register _register1;
  Register _register2;
  int64_t _offset;
  int64_t _multiplier;
  bool _isMemoryAccess;
};

class Instruction {
public:
  Instruction() {}
  Instruction(const std::string & inst): instruction(inst) {}
  Instruction(const std::string & inst, const Operand & op1): instruction(inst), op1(op1) {}
  Instruction(const std::string & inst, const Operand & op1, const Operand & op2): instruction(inst), op1(op1), op2(op2) {}

  void setInstruction(const std::string & inst) {
    instruction = inst;
  }

  void setOp1(const Operand & op) {
    op1 = op;
  }

  void setOp2(const Operand & op) {
    op2 = op;
  }

  friend std::ostream& operator<<(std::ostream& os, const Instruction& inst) {
    os << inst.instruction;
//    if (inst.instruction == "JMP") assert(inst.op1.has_value());
    if (inst.op1.has_value()) {
      os << " " << inst.op1.value();
    }
    if (inst.op2.has_value()) {
      assert(inst.op1.has_value());
      os << ", " << inst.op2.value();
    }
    return os;
  }

  const std::string & getInstruction() const{
    return instruction;
  }

private:
  std::string instruction;
  std::optional<Operand> op1;
  std::optional<Operand> op2;
};

#endif //TINYCC_INSTRUCTIONS_HPP
