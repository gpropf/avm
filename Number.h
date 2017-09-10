#ifndef Number_h
#define Number_h

class Number: public Cell {
  public:
    Number();
    ~Number();
    virtual int toInt() = 0;
    boolean exec(VM & vm);
};


class Int32: public Number {
  private:
    int32_t _ival;
  public:
    int toInt();
    Int32();
    Int32(int i);
    ~Int32();
    //void exec(VM & vm);
};

#endif
