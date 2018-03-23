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



enum class Opcode : uint8_t {
  MATH_BASE = 0,
  ADD_INT8 = MATH_BASE,

  SUB_INT8 = MATH_BASE + 1,
  MUL_INT8 = MATH_BASE + 2,
  DIV_INT8 = MATH_BASE + 3,
  OR8 = MATH_BASE + 4,
  AND8 = MATH_BASE + 5,
  NOT8 = MATH_BASE + 6,
  SHL8 = MATH_BASE + 7,
  SHR8 = MATH_BASE + 8,





  MOV_BASE = MATH_BASE + 9,
  MOV_REG2_MEM = MOV_BASE,
  MOV_MEM2_REG = MOV_BASE + 1,
  MOV_REG2_SPREL = MOV_BASE + 2,
  MOV_SPREL2_REG = MOV_BASE + 3,
  MOV_REG_IND2_SPREL = MOV_BASE + 4,
  MOV_SPREL_IND2_REG = MOV_BASE + 5,

  MOV_REG_IND2_REG = MOV_BASE + 6,
  MOV_REG_IND2_REG_IND = MOV_BASE + 7,
  MOV_REG2_REG_IND = MOV_BASE + 8,
  MOV_REG2_REG = MOV_BASE + 9,

  PUSH8_BASE = MOV_BASE + 10,
  PP_START = PUSH8_BASE,
  PUSH8_MEM = PUSH8_BASE,
  PUSH8_SPREL = PUSH8_BASE + 1,
  PUSH8_REGS = PUSH8_BASE + 2, // push one or two registers [R4,R4], if both are the same register then push only that one.
  PUSH8_REGS_IND = PUSH8_BASE + 3,
  PUSH8_CONST = PUSH8_BASE + 4,

  POP8_BASE = PUSH8_BASE + 5,
  POP8_REGS = POP8_BASE, // Same as push, if there are two differe nt regs this pops 2 values.
  PP_END = POP8_REGS,

  CMP_BASE = PP_END + 1,
  CMP_INT8 = CMP_BASE,
  CMP_UINT8 = CMP_BASE + 1,


  CMP_FLOAT = CMP_BASE + 2,
  CMP_STRING = CMP_BASE + 3,

  JUMP_BASE = CMP_BASE + 4,
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




  // PUSH/POP from/to A16
  /*
    PP_START = 15,

    PUSH8_MEM = 2 * PUSH8_BASE,
    PUSH8_SPREL = 17,
    PUSH8_CONST = 18,
    PUSH8_SPREL_IND = 19,
    PUSH8_REG = 20,

    PUSH16_MEM = 21,
    PUSH16_SPREL = 22,
    PUSH16_CONST = 23,
    PUSH16_SPREL_IND = 24,
    PUSH16_REG = 25,

    PUSH32_MEM = 26,
    PUSH32_SPREL = 27,
    PUSH32_CONST = 28,
    PUSH32_SPREL_IND = 29,
    PUSH32_REG = 30,

    POP8_REGS = 32,
    POP8_SPREL = 33,
    POP8_REGS_IND = 34,
    POP8_SPREL_IND = 35,
    POP8_REG = 36,

    POP16_MEM = 37,
    POP16_SPREL = 38,
    POP16_MEM_IND = 39,
    POP16_SPREL_IND = 40,
    POP16_REG = 41,

    POP32_MEM = 42,
    POP32_SPREL = 43,
    POP32_MEM_IND = 44,
    POP32_SPREL_IND = 45,
    POP32_REG = 46,

    PP_END = 47,

    MOV8_REG2_SPREL = 48,
    MOV16_REG2_SPREL = 49,
    MOV32_REG2_SPREL = 50,
    MOV8_SPREL2_REG = 51,
    MOV16_SPREL2_REG = 52,
    MOV32_SPREL2_REG = 53,

    MOV8_ADDR_REG2_REG = 54, // Treat the value in RA as an address and move the data found there to RB
    MOV16_ADDR_REG2_REG = 54,
    MOV32_ADDR_REG2_REG = 54,


    REL_MODE, // Set relative addressing mode (i.e. addresses are pointers)
    ABS_MODE, // Set absolute addressing mode (i.e. addresses are locations of data)
    DATA_INT8,
    DATA_INT16,
    DATA_INT32,
    DATA_UINT8,
    DATA_UINT16,
    DATA_UINT32,
    DATA_FLOAT,
    DATA_STRING,
  */
  NOOP,
  CALL, // Takes a uint16_t address of the function to call. Automatically saves return address
  RET, // Uses stored return address and leaves return value on stack



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
    AddressingMode _am;
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


  public:
    void writeString(char * sptr, uint16_t inAddr = 0, boolean advanceIP = true);
    String getDataTypeAndWidthString(DataMode dm = DataMode::INVALID_MODE);
    static const uint16_t DATA_SEG = 100;
    static const uint8_t dataWidth[];
    static uint8_t getDataWidth(DataMode dm);


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
      if (c >= Opcode::PP_START && c <= Opcode::PP_END) {
        switch (c) {
          case Opcode::PUSH8_MEM:
            writeData(a1, _ip16);
            break;
          case Opcode::POP8_REGS:
            writeData(a2, _ip16);
            break;
        }
      }
      else {
        switch (c) {
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
