#ifndef Instructions_h
#define Instructions_h



/*
class Pop: public Cell {
  public:
    Pop();
    ~Pop();
    void exec(VM & vm);
};

class Push: public Cell {
  public:
    Push();
    ~Push();
    void exec(VM & vm);
};
*/

class Halt: public Cell {
  public:
   
    boolean exec(VM & vm);
};

class Instruction: public Cell {
  public:
   
    boolean exec(VM & vm);
};


class Add2: public Instruction {
  public:
   
    boolean exec(VM & vm);
};

class AnalogRead: public Instruction {
  public:
   
    boolean exec(VM & vm);
};

class DigitalRead: public Instruction {
  public:
   
    boolean exec(VM & vm);
};


class JmpIfGreater: public Instruction {
  public:
   
    boolean exec(VM & vm);
};

#endif

