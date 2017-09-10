#ifndef VM_h
#define VM_h

class Cell;
class VM {
  private:
    Cell ** _stack;
    Cell ** _mem;
    uint16_t _memSize, _stackSize, _IP, _SP;
  public:

    uint8_t readPin(uint8_t pin, boolean isAnalog);
    VM(): _IP(0), _SP(0) {};
    VM(uint16_t memSize, uint16_t stackSize);
    void writeCell(Cell * c, uint16_t i);
    void step();
    Cell * pop();
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
