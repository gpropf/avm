//#include "VM.h"

#ifndef Cell_h
#define Cell_h



//class VM;
class Cell {

  public:
    Cell();
    virtual ~Cell();
    virtual boolean exec(VM & vm);
    /* The boolean return value indicates whether or not to increament the _IP after running
     *  the exec method. Most of the time this will be set to true which means the caller does
     *  the increment. Some instructions, like the Jmp and Halt ones though need to set this to
     *  false so that these instructions have the expected result.
     */
    //{
    //return 0;
    //};
};


#endif
