#ifndef VM_h
#define VM_h

/*
  enum class AddressingMode : uint8_t {
  REL, ABS
  };
*/
enum class Location : uint8_t {
  MEM, SPREL, MEM_IND, SPREL_IND, REG,
};

enum class Comparison : uint8_t {
  LESS_THAN = 1,
  GREATER_THAN = 2,
  EQUAL = 3,
};

enum class DataMode : uint8_t {

  UINT8 = 0,
  UINT16 = 1,
  UINT32 = 2,
  INT8 = 3,
  INT16 = 4,
  INT32 = 5,
  FLOAT = 6,
  STRING = 7,
  INVALID_MODE = 8,

};

struct RegPair {
  uint8_t reg1;
  uint8_t reg2;
};


#include "instruction_set.h"
/*
  enum class Opcode : uint8_t {
  MATH_BASE_8 = 0,
  ADD_UINT_8 = MATH_BASE_8,
  SUB_UINT_8 = MATH_BASE_8 + 1,
  MUL_UINT_8 = MATH_BASE_8 + 2,
  DIV_UINT_8 = MATH_BASE_8 + 3,
  OR_8   = MATH_BASE_8 + 4,
  AND_8 = MATH_BASE_8 + 5,
  NOT_8 = MATH_BASE_8 + 6,
  SHL_8 = MATH_BASE_8 + 7,
  SHR_8 = MATH_BASE_8 + 8,

  MOV_BASE_8 = MATH_BASE_8 + 9,
  MOV_REG2_MEM_8 = MOV_BASE_8,
  MOV_MEM2_REG_8 = MOV_BASE_8 + 1,
  MOV_REG2_SPREL_8 = MOV_BASE_8 + 2,
  MOV_SPREL2_REG_8 = MOV_BASE_8 + 3,
  MOV_REG_IND2_SPREL_8 = MOV_BASE_8 + 4,
  MOV_SPREL_IND2_REG_8 = MOV_BASE_8 + 5,

  MOV_REG_IND2_REG_8 = MOV_BASE_8 + 6,
  MOV_REG_IND2_REG_IND_8 = MOV_BASE_8 + 7,
  MOV_REG2_REG_IND_8 = MOV_BASE_8 + 8,
  MOV_REG2_REG_8 = MOV_BASE_8 + 9,

  PUSH_BASE_8 = MOV_BASE_8 + 10,
  // PP_START_8 = PUSH_BASE_8,
  PUSH_MEM_8 = PUSH_BASE_8,
  PUSH_SPREL_8 = PUSH_BASE_8 + 1,
  PUSH_REGS_8 = PUSH_BASE_8 + 2, // push one or two registers [R4,R4], if both are the same register then push only that one.
  PUSH_REGS_IND_8 = PUSH_BASE_8 + 3,
  PUSH_CONST_8 = PUSH_BASE_8 + 4,

  POP_BASE_8 = PUSH_BASE_8 + 5,
  POP_REGS_8 = POP_BASE_8, // Same as push, if there are two differe nt regs this pops 2 values.
  //PP_END_8 = POP_REGS_8,

  CMP_BASE = POP_BASE_8 + 1,
  CMP_INT_8 = CMP_BASE,
  CMP_UINT_8 = CMP_BASE + 1,

  INC_BASE = CMP_BASE + 2,
  INC_SPREL_UINT_8 = INC_BASE + 1,
  INC_SPREL_INT_8 = INC_BASE + 2,

  //INC_END = INC_SPREL_INT_8,

  END_8 = INC_SPREL_INT_8,





  // **************************************
  // Below this we only need one of each instruction since there are not multiple data widths.
  FIXED_WIDTH_BASE = 200,
  CMP_FLOAT = FIXED_WIDTH_BASE,
  CMP_STRING = FIXED_WIDTH_BASE + 1,

  JUMP_BASE = FIXED_WIDTH_BASE + 2,
  JNE = JUMP_BASE,
  JEQ = JUMP_BASE + 1,
  JLT = JUMP_BASE + 2,
  JGT = JUMP_BASE + 3,
  UJMP = JUMP_BASE + 4, // Unconditional jump

  BIND_BASE = JUMP_BASE + 5,
  BINDAI = BIND_BASE, // A16 V8: Bind a mem address (uint16_t) (analog input) to a pin (uint8_t)
  BINDDI = BIND_BASE + 1, // A16 V8: Bind a mem address (uint16_t) (digital input) to a pin (uint8_t)
  BINDAO = BIND_BASE + 2, // A16 V8: Bind a mem address (uint16_t) (analog out) to a pin (uint8_t)
  BINDDO = BIND_BASE + 3, // A16 V8: Bind a mem address (uint16_t) (digital output) to a pin (uint8_t)
  BINDAP = BIND_BASE + 4, // A16 V8: Bind a mem address (uint16_t) (analog input-pullup) to a pin (uint8_t)
  BINDDP = BIND_BASE + 5, // A16 V8: Bind a mem address (uint16_t) (digital input-pullup) to a pin (uint8_t)


  SP_ADJ = BIND_BASE + 6, // increment the SP without pop. Takes uint8_t as arg.
  PRINT_AS = BIND_BASE + 7, // Takes an 8 bit operand. First nibble is register to print, 2nd is type to print as.
  NOOP = 249,
  NOOP_INIT = 250,
  CALL = 255, // Takes a uint16_t address of the function to call. Automatically saves return address
  RET = 254, // Uses stored return address and leaves return value on stack



  };
*/

struct OpcodeAndDataWidth {
  Opcode c;
  uint8_t dw;
};

template <typename T>
struct doublePointer {
  T *srcreg;
  T *destreg;
};

class VM {

    class PinBinding {

        uint8_t _pin;
        uint16_t _address;
        boolean _ad; // true = analog, false = digital
        uint8_t _io; // INPUT, OUTPUT, INPUT_PULLUP
      public:
        uint16_t read();
        void write(uint16_t val);
        void setAddress(uint16_t address);
        boolean getAD();
        void setAD(boolean aord);
        uint8_t getIO();
        void setIO(uint8_t io);
        void setPin(uint8_t pin);
        uint8_t getPin();
        void updatePin(VM & vm);
        void print();

        PinBinding();
        PinBinding(uint8_t pin): _pin(pin) {};

    };

  private:

    PinBinding _pinBindings[NUM_PINS];
    //DataMode _dm;

    uint16_t _ip16Copy;
    Comparison _cmpReg;

    uint8_t _reg[64];

    //uint8_t * _progmem;
    uint16_t _memSize, _stackSize, _SP, _AP;
    // _AP is "append pointer, used at the beginning to make it easy to push a bunch
    // of instructions and data into the memory.
    int16_t getStringLength(char * startAddr);
    static constexpr char * dmString = "UIFS";
    //static const String dwStrings[3] = {"8", "16", "32"};
    static const char* _dataModeStrings[8];
    uint8_t * getPtr(uint16_t addr, Location locationType);
    void loadRegWithConst(uint8_t reg, uint32_t c = 0);
    RegPair getRegPair();
    uint16_t _ip16;
    String OpcodeWithWidth2String(OpcodeAndDataWidth opdw); // Might not do this after all.

  public:
    uint8_t * _mem;
    VM(): _ip16(0), _SP(0) , _AP(0) {};
    VM(uint16_t memSize, uint16_t stackSize);



    String getAsString(uint8_t* addr8, const DataMode dm);
    String getAsString(uint16_t addr, const String modeString);
    String getAsString(uint16_t addr, const DataMode dm);
    String getAsString(uint8_t regnum, const DataMode dm);


    void writeString(char * sptr, uint16_t inAddr = 0, boolean advanceIP = true);

    static const uint16_t DATA_SEG = 100;
    static const uint8_t dataWidth[];
    //  static uint8_t getDataWidth(DataMode dm);
    static OpcodeAndDataWidth getOpcodeAndDataWidth(Opcode c);
    static Opcode getOpcodeByDataWidth(Opcode c, uint8_t dw);

    /* ===============================================
        Pin binding code
       ===============================================
    */
    void createBinding(uint8_t pin, uint8_t io, boolean ad, uint16_t addr);
    void updateBoundData();
    // ************************************************


    void moveData(uint8_t * srcptr, uint8_t * destptr, uint8_t datumWidth);

    // The zero in changeIP acts as a semaphor value causing the address to reset to its original value.
    void changeIP(int16_t addressDelta = 0);

    inline void setIP(uint16_t newIP) {
      _ip16 = newIP;
    }

    void setSP(uint16_t newIP);
    inline uint16_t getIP() {
      return _ip16;
    }

    inline void incIP() {
      _ip16++;
    }
    void reset();

    uint8_t readMem(uint16_t i);
    void step();
    void exec(Opcode opcode);

    //String getDataTypeAndWidthString(DataMode dm = DataMode::INVALID_MODE);
    void printMem(uint16_t startAddr, uint16_t endAddr, boolean printAsCArray = false);
    void printRegisters();
    void printStatus();
    void printStack();
    void printBindings();

    /* =================================================
        Template code for variable type and width data
       =================================================
    */
    template <class datum>
    doublePointer<datum> getRegPair2() {
      doublePointer<datum> dp;
      uint8_t * srcptr;
      uint8_t * destptr;
      RegPair tr = getRegPair();
      srcptr = getPtr(tr.reg1, Location::REG);
      destptr = getPtr(tr.reg2, Location::REG);
      dp.srcreg = reinterpret_cast<datum*>(srcptr);
      dp.destreg = reinterpret_cast<datum*>(destptr);
      return dp;
    }


    template <class datum>
    void castRegData(datum &value1, datum &value2, RegPair rp) {
      value1 = *(reinterpret_cast<datum*>(&(_reg[rp.reg1 * 4])));
      value2 = *(reinterpret_cast<datum*>(&(_reg[rp.reg2 * 4])));
    }

    template <class datum> datum readDataAtPtr(datum *inAddr)
    {

      datum * dptr = reinterpret_cast<datum*>(inAddr);
      datum d = *dptr;
      return d;
    }

    template <class datum> datum readData(uint16_t inAddr = 0, boolean advanceIP = true)
    {
      if (advanceIP) {
        inAddr = _ip16;
        _ip16 += sizeof(datum);
      }
      /*
            datum * dptr = reinterpret_cast<datum*>(&_mem[inAddr]);
            datum d = *dptr;
            return d;
      */
      datum * dptr = reinterpret_cast<datum*>(&_mem[inAddr]);
      datum d = readDataAtPtr<datum>(dptr);
      return d;
    }



    template <class datum>
    void writeData(datum d, uint16_t inAddr = 0, boolean writeAtIP = true, boolean advanceIP = true) {
      if (writeAtIP) {
        inAddr = _ip16;

      }
      if (advanceIP)
        _ip16 += sizeof(datum);
      datum * dptr = reinterpret_cast<datum*>(&_mem[inAddr]);
      *dptr = d;
    }

    template <class A1 = uint16_t, class A2 = uint8_t, class A3 = uint8_t>
    void writeInstruction(Opcode c, A1 a1 = 0, A2 a2 = 0, A3 a3 = 0)
    {
      writeData(c);
      OpcodeAndDataWidth opPair = getOpcodeAndDataWidth(c);

      // This switch only does something if we have an instruction with an argument
      switch (opPair.c) {
        case Opcode::SP_ADJ: {
            writeData(a2, _ip16);
            break;
          }
        case Opcode::INC_SPREL_UINT_8: {
            writeData(a2, _ip16);
            break;
          }
        case Opcode::MOV_SPREL2_REG_8: {
            writeData(a2, _ip16);
            writeData(a3, _ip16);
            break;
          }
        case Opcode::MOV_REG2_SPREL_8: {
            writeData(a2, _ip16);
            writeData(a3, _ip16);
            break;
          }
        case Opcode::PUSH_CONST_8:
          writeData(a2, _ip16);
          break;
        case Opcode::CMP_INT_8:
          writeData(a2, _ip16);
          break;
        case Opcode::UJMP:
          writeData(a1, _ip16);
          break;
        case Opcode::JEQ:
          writeData(a1, _ip16);
          break;
        case Opcode::CALL:
          writeData(a1, _ip16);
          break;
        case Opcode::PUSH_MEM_8:
          writeData(a1, _ip16);
          break;
        case Opcode::POP_REGS_8:
          writeData(a2, _ip16);
          break;
        case Opcode::ADD_UINT_8:
        case Opcode::MUL_UINT_8:
        case Opcode::DIV_UINT_8:
        case Opcode::SUB_UINT_8:
          writeData(a2, _ip16);
          break;
        case Opcode::BINDAO:
        case Opcode::BINDDO:
        case Opcode::BINDDI:
        case Opcode::BINDDP:
        case Opcode::BINDAP:
        case Opcode::BINDAI: {
            writeData(a1, _ip16);
            //_ip16 += (sizeof(a1));
            writeData(a2, _ip16);
            //_ip16 += (sizeof(a2));
          }
          break;
      }
    }
};

#endif
