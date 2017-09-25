//#include "VM.h"

#ifndef Cell_h
#define Cell_h

enum CellType: uint8_t {
  CT_undef = 0,
  CT_i = 1,
  CT_f = 2,
  CT_str = 3,
  add2 = 4,
  mul2,
  div2,
  repeat,
  bindAI,
  bindAO,
  bindDI,
  bindDO,
  delay_t,
  jig,
  jeq,
  jneq,
  jilt,
  push, 
  // push: the memory address is on top of the stack, this is popped and the value at the
  // address is pushed. Pop pops the address and then pops the next value on the stack
  // and puts it into that address.
  
  pop
};

//class VM;
class Cell {
  public:
  
    union Data {
      
      int32_t ival;
      float fval;
      String * stringVal;
    };
    
  
    Cell();
    Cell(CellType ct, Data d);
    Cell(CellType ct);
    virtual ~Cell();
    virtual boolean exec(VM & vm);
    void print();
    /* The boolean return value indicates whether or not to increament the _IP after running
        the exec method. Most of the time this will be set to true which means the caller does
        the increment. Some instructions, like the Jmp and Halt ones though need to set this to
        false so that these instructions have the expected result.
    */
    //{
    //return 0;
    //};
    
    Data _d;
    CellType _ct;
};


#endif
