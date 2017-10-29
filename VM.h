#ifndef VM_h
#define VM_h

enum class Opcode : uint8_t {
  ADD = 0b00, // Math operation
  SUB = 0b01,
  MUL = 0b10,
  DIV = 0b11,

  INT = 0b0000, // Data type operated upon
  FLT = 0b0100,
  STR = 0b1100,

  JNE = 0b000000, // Conditionals
  JEQ = 0b010000,
  JLT = 0b100000,
  JGT = 0b110000,

  BINDAO, // V8, A16: Bind a pin (uint8_t) to a mem address (uint16_t) (analog out)
  BINDAI, // V8, A16: Bind a pin (uint8_t) to a mem address (uint16_t) (analog input)
  BINDDO, // V8, A16: Bind a pin (uint8_t) to a mem address (uint16_t) (digital output)
  BINDDI, // V8, A16: Bind a pin (uint8_t) to a mem address (uint16_t) (digital input)
  BINDDP, // V8, A16: Bind a pin (uint8_t) to a mem address (uint16_t) (digital input-pullup)
  BINDAP, // V8, A16: Bind a pin (uint8_t) to a mem address (uint16_t) (analog input-pullup)
  PUSH8, // A16: Push an 8 bit value at mem address (uint16_t) onto the stack
  PUSH16, // A16: Push a 16 bit value at mem address (uint16_t) onto the stack
  PUSH32, // A16: Push a 16 bit value at mem address (uint16_t) onto the stack
  POP8, // A16: Pop the 8 bit value on the top of the stack into memaddress
  POP16, // A16: Pop the 16 bit value on the top of the stack into memaddress
  POP32, // A16: Push a 16 bit value at mem address (uint16_t) onto the stack
};



/*
  struct Opcode {
  int x1 : 8 = 42;:

  // int foo : 8 = 0;
  operation: 4;
  : 2;

  void process(VM &vm);
  };
*/

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
    void updateBoundData();

    inline uint16_t getIP() {
      return _ip16;
    }

    inline void incIP() {
      //_IP++;
      _ip16++;
    }

    void exec(Opcode opcode);

    inline void setIP(uint16_t newIP) {
      dprintln("New IP:" + String(newIP));
      //_IP = newIP;
      _ip16 = newIP;
    }

    inline void setSP(uint16_t newIP) {
      dprintln("New SP:" + String(newIP));
      _SP = newIP;
    }

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

    template <class A1 = uint8_t, class A2 = uint16_t, class A3 = int32_t>
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
      }
    }

    /*
        template <class datum> writeData(datum d, uint16_t inAddr = 0, boolean advanceMemAddr = true)
        {
          // advanceMemAddr: if true we are using the _ip16 as the address. inAddr is ignored in
          // this case. If advanceMemAddr is false we are using the inAddr.

          uint16_t addr;
          uint8_t step = 0;
          if (advanceMemAddr) {
            addr = _ip16;
            step = sizeof(datum);
          }
          else {
            addr = inAddr;
          }
          datum * dptr = reinterpret_cast<datum*>(&_mem[addr]);
           dptr = d;

          _ip16 += step;

        }
    */

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
    /*
        template <class datum> datum readData(uint16_t address = 0, boolean advanceMemAddr = true)
        {
          datum * dptr = reinterpret_cast<datum*>(&_mem[_ip16]);
          datum d = *dptr;

          if (advanceMemAddr) {
            _ip16 += sizeof(datum);
          }
          return d;
        }
    */
};

// ===========================================
// Deprecated Code Below This Line
// ===========================================

//void setPinIO(uint8_t pin, uint8_t m);
//void setPinAD(uint8_t pin, boolean ad);
//void setPinAddress(uint8_t pin, uint16_t address);

//uint8_t readPin(uint8_t pin, boolean isAnalog);




#endif
