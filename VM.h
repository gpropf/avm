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


enum class stackElementType : uint8_t {
  // First two bits determines type, uint, int, float, or string

  UINT = 0b00,
  INT = 0b01,
  FLOAT = 0b10,
  STRING = 0b11,

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

struct stackElement {
  stackElementType t;
  stackElementData d;

  //stackElement(stackElementType t, stackElementData d): t(t), d(d) {};
};

enum class AddressingMode : uint8_t {
  REL, ABS
};

enum class DataMode : uint8_t {
  INT8, INT16, INT32,
  UINT8, UINT16, UINT32,
  FLOAT, STRING
};

enum class Opcode : uint8_t {
  ADD = 1, // Math operation on top 2 stack elements
  SUB = 2,
  MUL = 3,
  DIV = 4,

  // INT = 0b0000, // Data type operated upon
  // FLT = 0b0100,
  // STR = 0b1100,

  JNE = 0b000000, // Takes a 16 bit address or pointer
  JEQ = 0b010000, // Takes a 16 bit address or pointer
  JLT = 0b100000, // Takes a 16 bit address or pointer
  JGT = 0b110000, // Takes a 16 bit address or pointer


  BINDAI, // A16 V8: Bind a mem address (uint16_t) (analog input) to a pin (uint8_t)
  BINDDI, // A16 V8: Bind a mem address (uint16_t) (digital input) to a pin (uint8_t)
  BINDAO, // A16 V8: Bind a mem address (uint16_t) (analog out) to a pin (uint8_t)
  BINDDO, // A16 V8: Bind a mem address (uint16_t) (digital output) to a pin (uint8_t)
  BINDAP, // A16 V8: Bind a mem address (uint16_t) (analog input-pullup) to a pin (uint8_t)
  BINDDP, // A16 V8: Bind a mem address (uint16_t) (digital input-pullup) to a pin (uint8_t)

  // PUSH/POP from/to A16
  PUSH, // Push 1, 2, or 4 bytes or a null terminated string according to data mode
  POP, // Pop 1, 2, or 4 bytes or a null terminated string according to data mode

  REL_MODE, // Set relative addressing mode (i.e. addresses are pointers)
  ABS_MODE, // Set absolute addressing mode (i.e. addresses are locations of data)
  DATA_INT8,
  DATA_INT16,
  DATA_INT32,
  DATA_UINT8,
  DATA_UINT16,
  DATA_UINT32,
  DATA_FLOAT,
  DATA_STRING



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
    uint8_t * _stack;

    uint8_t * _mem;
    uint16_t _ip16;
    uint16_t _ip16Copy;

    //uint8_t * _progmem;
    uint16_t _memSize, _stackSize, _SP, _AP;
    // _AP is "append pointer, used at the beginning to make it easy to push a bunch
    // of instructions and data into the memory.

  public:

    void createBinding(uint8_t pin, uint8_t io, boolean ad, uint16_t addr);
    // void writeByte(uint8_t i32, boolean advanceMemAddr);
    void changeIP(int16_t addressDelta = 0);
    VM(): _ip16(0), _SP(0) , _AP(0) {};
    VM(uint16_t memSize, uint16_t stackSize);
    uint8_t readMem(uint16_t i);
    void step();
    void printMem(uint16_t startAddr, uint16_t endAddr);
    void printStatus();
    void updateBoundData();
    void transferData(uint16_t addr, uint8_t * buf, DataMode dm, boolean toStack,
                            boolean adjustSP = true, boolean alterMemory = true);
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
    void writeDataToStackOrMem(datum d, uint16_t addr = 0, uint16_t * pointer = NULL) {
      // addr: actual location in memory to write to in either stack or mem array
      // pointer: a bit confusingly this is a pointer to a pointer, either SP or IP
      // the relevant pointer is advanced by the size of the datum.

      datum * dptr = reinterpret_cast<datum*>(addr);
      *dptr = d;
      if (pointer) {
        (*pointer) += sizeof(datum);
      }
    }



    template <class datum>
    void push(datum d) {
      writeDataToStackOrMem(d, &_stack[_SP], &_SP );
    }

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
        case Opcode::PUSH:
          writeData(a1, _ip16);
          break;
        case Opcode::POP:
          writeData(a1, _ip16);
          break;
      }
    }

    template <class datum> datum readData(uint16_t inAddr = 0, boolean advanceIP = true)
    {
      if (advanceIP) {
        inAddr = _ip16;
        _ip16 += sizeof(datum);
      }
      datum * dptr = reinterpret_cast<datum*>(&_mem[inAddr]);
      datum d = *dptr;
      return d;
    }
};


#endif
