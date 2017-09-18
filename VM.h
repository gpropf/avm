#ifndef VM_h
#define VM_h

class Cell;
class Int32;
class VM {
    class PinBinding {
        uint8_t _pin;
        uint16_t _address;
        boolean _ad; // true = analog, false = digital
        uint8_t _mode; // INPUT, OUTPUT, INPUT_PULLUP
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
    };
  private:
    PinBinding bindings[NUM_PINS];

    Cell ** _stack;
    Cell ** _mem;
    uint16_t _memSize, _stackSize, _IP, _SP, _AP;
    // _AP is "append pointer, used at the beginning to make it easy to push a bunch
    // of instructions and data into the memory.
  public:
    void setPinIO(uint8_t pin, uint8_t m);
    void setPinAD(uint8_t pin, boolean ad);
    void setPinAddress(uint8_t pin, uint16_t address);
    void loadBinding(Int32 * pinCell, Int32 * memCell);
    uint8_t readPin(uint8_t pin, boolean isAnalog);
    VM(): _IP(0), _SP(0) , _AP(0) {};
    VM(uint16_t memSize, uint16_t stackSize);
    void writeCell(Cell * c, uint16_t i);
    void step();
    Cell * pop();
    void appendCell(Cell * c);
    inline uint16_t getIP() {
      return _IP;
    }
    inline void incIP() {
      _IP++;
    }
    inline void setIP(uint16_t newIP) {
      Serial.println("New IP:" + String(newIP));
      _IP = newIP;
    }
    inline void setSP(uint16_t newIP) {
      Serial.println("New SP:" + String(newIP));
      _SP = newIP;
    }
    void push(Cell *c);
    void printStack();

    void reset();
};

#endif
