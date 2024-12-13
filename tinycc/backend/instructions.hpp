
#ifndef TINYCC_INSTRUCTIONS_HPP
#define TINYCC_INSTRUCTIONS_HPP

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
#define R0 5
#define R1 6

#define MAIN 2

typedef size_t Register;
class Operand {

public:
  Operand(Register r, int64_t offset): _register(r), _offset(offset), _isMemoryAccess(false) {}
  Operand(Register r): _register(r), _offset(0), _isMemoryAccess(false) {
    assert(r != FLAGS);
  }

  Operand & setMemoryAccess() {
    _isMemoryAccess = true;
    return *this;
  }

  Operand & setOffset(int64_t offset) {
    _offset = offset;
    return *this;
  }

  Operand & setRegister(Register reg) {
    _register = reg;
    return *this;
  }

  friend std::ostream& operator<<(std::ostream& os, const Operand & op) {
    if (op._isMemoryAccess) os << "[";
    if (op._register != NOREG) {
      switch (op._register) {
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
          assert(op._register > R1);
          os << "R" << op._register - R1 + 1;
          break;
      }
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

private:
  Register _register;
  int64_t _offset;
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
    if (inst.op1.has_value()) {
      os << " " << inst.op1.value();
    }
    if (inst.op2.has_value()) {
      assert(inst.op1.has_value());
      os << ", " << inst.op2.value();
    }
    return os;
  }

private:
  std::string instruction;
  std::optional<Operand> op1;
  std::optional<Operand> op2;
};

#endif //TINYCC_INSTRUCTIONS_HPP
