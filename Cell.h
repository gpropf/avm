//#include "VM.h"

#ifndef Cell_h
#define Cell_h



//class VM;
class Cell {

  public:
    Cell();
    virtual ~Cell();
    virtual void exec(VM & vm);
    //{
    //return 0;
    //};
};


#endif
