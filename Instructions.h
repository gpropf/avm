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


class Instruction: public Cell {
  public:
   
    void exec(VM & vm);
};


class Add2: public Instruction {
  public:
   
    void exec(VM & vm);
};


class JmpIfGreater: public Instruction {
  public:
   
    void exec(VM & vm);
};

#endif

