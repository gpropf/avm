#ifndef VM_h
#define VM_h

class Cell;
class VM {
  private:
    Cell ** _stack;
    Cell ** _mem;
    uint16_t _memSize, _stackSize, _IP, _SP;
  public:
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

    void push(Cell *c);
};

#endif
