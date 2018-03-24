#ifndef VM_h
#define VM_h

/*
   Some Notes on Opcodes and Type System
   --------------------------------------

   Types: uint8, uint16, uint32, int8, int16, int32, float32, string: 3 bits
   Stack Operations: push, pop (types determined by type field): 1 bit

  Above must be grouped as we must know *what* we are pushing and popping and
  this information is not stored with the values (unless we implement our typed stack idea)

   Math Operators: add, sub, mul, div: 2 bits
   Binding Operations: AI, DI, AO, DO, AP, DP: 6 possibilities can be organized by odd/even.
   Perhaps combine binding ops with push/pop to make a single 3 bit field.


   Branching: jne, jeq, jlt, jgt (Jump if not equal, equal, less than, and greater than respectively): 2 bits

  It looks like a single bit could be used to switch between the typed push/pop instructions
  and the others.


*/
/*
  enum class stackElementType : uint8_t {
  // First two bits determines type, uint, int, float, or string

  UINT = 0b0000,
  INT = 0b0001,
  FLOAT = 0b0010,
  STRING = 0b0011,

  // Second two bits determines width, 8, 16, or 32 bits. Strings have varying
  // length.

  WIDTH8 = 0b0100,
  WIDTH16 = 0b1000,
  WIDTH32 = 0b1100,

  // Some common types

  UINT8 = UINT & WIDTH8,
  UINT16 = UINT & WIDTH16,
  UINT32 = UINT & WIDTH32,

  INT8 = INT & WIDTH8,
  INT16 = INT & WIDTH16,
  INT32 = INT & WIDTH32,

  };
*/
/*
  union stackElementData {
  uint8_t ui8;
  uint16_t ui16;
  uint32_t ui32;
  int8_t i8;
  int16_t i16;
  int32_t i32;
  char * str;
  float fl;
  double dbl;
  };
*/
/*
  struct stackElement {
  stackElementType t;
  stackElementData d;

  //stackElement(stackElementType t, stackElementData d): t(t), d(d) {};
  };
*/

enum class AddressingMode : uint8_t {
  REL, ABS
};

enum class Location : uint8_t {
  /*
    R0 = 0,
    R1 = 1,
    R2 = 2,
    R3 = 3,
    R4 = 4,
    R5 = 5,
    R6 = 6,
    R7 = 7,

    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,

    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
  */
  MEM, SPREL, MEM_IND, SPREL_IND, REG,
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

enum class Opcode : uint8_t {
  MATH_BASE_8 = 0,
  ADD_INT_8 = MATH_BASE_8,
  SUB_INT_8 = MATH_BASE_8 + 1,
  MUL_INT_8 = MATH_BASE_8 + 2,
  DIV_INT_8 = MATH_BASE_8 + 3,
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
  PP_START_8 = PUSH_BASE_8,
  PUSH_MEM_8 = PUSH_BASE_8,
  PUSH_SPREL_8 = PUSH_BASE_8 + 1,
  PUSH_REGS_8 = PUSH_BASE_8 + 2, // push one or two registers [R4,R4], if both are the same register then push only that one.
  PUSH_REGS_IND_8 = PUSH_BASE_8 + 3,
  PUSH_CONST_8 = PUSH_BASE_8 + 4,

  POP_BASE_8 = PUSH_BASE_8 + 5,
  POP_REGS_8 = POP_BASE_8, // Same as push, if there are two differe nt regs this pops 2 values.
  PP_END_8 = POP_REGS_8,

  CMP_BASE = PP_END_8 + 1,
  CMP_INT_8 = CMP_BASE,
  CMP_UINT_8 = CMP_BASE + 1,

  END_8 = CMP_UINT_8 + 1,


  // Below this we only need one of each instruction since there are not multiple data widths.
  FIXED_WIDTH_BASE = 200,
  CMP_FLOAT = FIXED_WIDTH_BASE,
  CMP_STRING = FIXED_WIDTH_BASE + 1,

  JUMP_BASE = FIXED_WIDTH_BASE + 2,
  JNE = JUMP_BASE,
  JEQ = JUMP_BASE + 1,
  JLT = JUMP_BASE + 2,
  JGT = JUMP_BASE + 3,

  BIND_BASE = JUMP_BASE + 4,
  BINDAI = BIND_BASE, // A16 V8: Bind a mem address (uint16_t) (analog input) to a pin (uint8_t)
  BINDDI = BIND_BASE + 1, // A16 V8: Bind a mem address (uint16_t) (digital input) to a pin (uint8_t)
  BINDAO = BIND_BASE + 2, // A16 V8: Bind a mem address (uint16_t) (analog out) to a pin (uint8_t)
  BINDDO = BIND_BASE + 3, // A16 V8: Bind a mem address (uint16_t) (digital output) to a pin (uint8_t)
  BINDAP = BIND_BASE + 4, // A16 V8: Bind a mem address (uint16_t) (analog input-pullup) to a pin (uint8_t)
  BINDDP = BIND_BASE + 5, // A16 V8: Bind a mem address (uint16_t) (digital input-pullup) to a pin (uint8_t)

  NOOP,
  CALL, // Takes a uint16_t address of the function to call. Automatically saves return address
  RET, // Uses stored return address and leaves return value on stack



};


struct OpcodeAndDataWidth {
  Opcode c;
  uint8_t dw;
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
    DataMode _dm;
    //AddressingMode _am;
    //uint8_t * _stack;

    uint8_t * _mem;
    uint16_t _ip16;
    uint16_t _ip16Copy;

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
    RegPair getRegPair(uint8_t registers);


  public:
    void writeString(char * sptr, uint16_t inAddr = 0, boolean advanceIP = true);
    String getDataTypeAndWidthString(DataMode dm = DataMode::INVALID_MODE);
    static const uint16_t DATA_SEG = 100;
    static const uint8_t dataWidth[];
    static uint8_t getDataWidth(DataMode dm);
    static OpcodeAndDataWidth getOpcodeAndDataWidth(Opcode c);
    static Opcode getOpcodeByDataWidth(Opcode c, uint8_t dw);


    void createBinding(uint8_t pin, uint8_t io, boolean ad, uint16_t addr);
    // void writeByte(uint8_t i32, boolean advanceMemAddr);
    void changeIP(int16_t addressDelta = 0);
    VM(): _ip16(0), _SP(0) , _AP(0) {};
    VM(uint16_t memSize, uint16_t stackSize);
    uint8_t readMem(uint16_t i);
    void step();
    void printMem(uint16_t startAddr, uint16_t endAddr);
    void printRegisters();
    void printStatus();
    void updateBoundData();
    void transferData(uint16_t addr, uint8_t * buf, DataMode dm, boolean toStack,
                      boolean adjustSP = true, boolean alterMemory = true);


    void moveData(uint8_t * srcptr, uint8_t * destptr, uint8_t datumWidth);
    inline uint16_t getIP() {
      return _ip16;
    }

    inline void incIP() {
      //_IP++;
      _ip16++;
    }

    void exec(Opcode opcode);

    void setIP(uint16_t newIP);

    void setSP(uint16_t newIP);

    void printStack();
    void printBindings();
    void reset();







    template <class datum>
    void writeData(datum d, uint16_t inAddr = 0, boolean advanceIP = true) {
      if (advanceIP) {
        inAddr = _ip16;
        _ip16 += sizeof(datum);
      }
      datum * dptr = reinterpret_cast<datum*>(&_mem[inAddr]);
      *dptr = d;
    }

    template <class A1 = uint16_t, class A2 = uint8_t, class A3 = int32_t>
    void writeInstruction(Opcode c, A1 a1 = 0, A2 a2 = 0, A3 a3 = 0)
    {

      writeData(c);

      OpcodeAndDataWidth opPair = getOpcodeAndDataWidth(c);



      switch (opPair.c) {

        case Opcode::PUSH_MEM_8:
          writeData(a1, _ip16);
          break;
        case Opcode::POP_REGS_8:
          writeData(a2, _ip16);
          break;
        case Opcode::ADD_INT_8:
        case Opcode::MUL_INT_8:
        case Opcode::DIV_INT_8:
        case Opcode::SUB_INT_8:
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
          /*
            case Opcode::PUSH:
            writeData(a1, _ip16);
            break;
            case Opcode::POP:
            writeData(a1, _ip16);
            break;
          */
      }

    }

    template <class datum> datum readData(uint16_t inAddr = 0, boolean advanceIP = true)
    {
      if (advanceIP) {
        inAddr = _ip16;
        _ip16 += sizeof(datum);
      }
      dprintln("IP = " + String(_ip16));
      datum * dptr = reinterpret_cast<datum*>(&_mem[inAddr]);
      datum d = *dptr;
      return d;
    }
};



#endif
