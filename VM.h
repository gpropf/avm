#ifndef VM_h
#define VM_h

/*
  enum class AddressingMode : uint8_t {
  REL, ABS
  };
*/
enum class Location : uint8_t {
  MEM, SPREL, MEM_IND, SPREL_IND, REG,
};

enum class Comparison : uint8_t {
  LESS_THAN = 1,
  GREATER_THAN = 2,
  EQUAL = 3,
};

enum class DataMode : uint8_t {

  UINT8 = 0,
  UINT16 = 1,
  UINT32 = 2,
  INT8 = 3,
  INT16 = 4,
  INT32 = 5,
  FLOAT = 6,
  STRING = 7,
  INVALID_MODE = 8,

};

struct RegPair {
  uint8_t reg1;
  uint8_t reg2;
};

#include "instruction_set.h"

struct OpcodeAndDataWidth {
  Opcode c;
  uint8_t dw;
};

template <typename T>
struct doublePointer {
  T *srcreg;
  T *destreg;
};

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

    VM * _nextVM = 0;
    PinBinding _pinBindings[NUM_PINS];
    //DataMode _dm;

    uint16_t _ip16Copy;
    Comparison _cmpReg;



    int16_t getStringLength(char * startAddr);
    //static constexpr char * dmString = "UIFS";
    //static const String dwStrings[3] = {"8", "16", "32"};
    static String const _dataModeStrings[];

    void loadRegWithConst(uint8_t reg, uint32_t c = 0);
    RegPair getRegPair();
    //uint16_t ;
    String OpcodeWithWidth2String(OpcodeAndDataWidth opdw); // Might not do this after all.
    //uint16_t translateAddr(const uint16_t addr) const;
    uint16_t readAddr();


  public:
    uint8_t * _mem;
    uint16_t _memBaseAddr, _SP;
    uint8_t * _reg;
    uint16_t _memSize, _stackSize, _ip16;


    VM(uint16_t memSize, uint16_t stackSize);
    VM(uint8_t * memBase, uint16_t memBaseAddr, uint16_t stackBaseAddr, uint8_t * regBase);
    uint8_t * getPtr(uint16_t addr, Location locationType);
    void printBootMsg(const String) const;

    String getAsString(uint8_t* addr8, const DataMode dm) const;
    String getAsString(uint16_t addr, const String modeString) const;
    String getAsString(uint16_t addr, const DataMode dm) const;
    String getAsString(uint8_t regnum, const DataMode dm) const;


    void writeString(char * sptr, uint16_t inAddr = 0, boolean advanceIP = true);

    static const uint16_t DATA_SEG = 100;
    static const uint8_t dataWidth[];
    //  static uint8_t getDataWidth(DataMode dm);
    static OpcodeAndDataWidth getOpcodeAndDataWidth(Opcode c);
    static Opcode getOpcodeByDataWidth(Opcode c, uint8_t dw);

    /* ===============================================
        Pin binding code
       ===============================================
    */
    void createBinding(uint8_t pin, uint8_t io, boolean ad, uint16_t addr);
    void updateBoundData();
    // ************************************************


    void moveData(uint8_t * srcptr, uint8_t * destptr, uint8_t datumWidth);

    // The zero in changeIP acts as a semaphor value causing the address to reset to its original value.
    void changeIP(int16_t addressDelta = 0);

    inline void setIP(uint16_t newIP) {
      _ip16 = newIP;
    }

    void setSP(uint16_t newIP);
    inline uint16_t getIP() {
      return _ip16;
    }

    inline void incIP() {
      _ip16++;
    }
    void reset();

    uint8_t readMem(uint16_t i);
    void step();
    void exec(Opcode opcode);

    //String getDataTypeAndWidthString(DataMode dm = DataMode::INVALID_MODE);
    void printMem(uint16_t startAddr, uint16_t endAddr, boolean printAsCArray = false);
    void printRegisters();
    void printStatus();
    void printStack();
    void printBindings();

    /* =================================================
        Template code for variable type and width data
       =================================================
    */
    template <class datum>
    doublePointer<datum> getRegPair2() {
      doublePointer<datum> dp;
      uint8_t * srcptr;
      uint8_t * destptr;
      RegPair tr = getRegPair();
      srcptr = getPtr(tr.reg1, Location::REG);
      destptr = getPtr(tr.reg2, Location::REG);
      dp.srcreg = reinterpret_cast<datum*>(srcptr);
      dp.destreg = reinterpret_cast<datum*>(destptr);
      return dp;
    }


    template <class datum>
    void castRegData(datum &value1, datum &value2, RegPair rp) {
      value1 = *(reinterpret_cast<datum*>(&(_reg[rp.reg1 * 4])));
      value2 = *(reinterpret_cast<datum*>(&(_reg[rp.reg2 * 4])));
    }

    template <class datum> datum readDataAtPtr(datum *inAddr) const {
      datum * dptr = reinterpret_cast<datum*>(inAddr);
      datum d = *dptr;
      return d;
    }

    template <class datum> datum readDataConst(uint16_t inAddr = 0) const {
      //inAddr = translateAddr(inAddr);
      datum * dptr = reinterpret_cast<datum*>(&_mem[inAddr]);
      datum d = readDataAtPtr<datum>(dptr);
      return d;

    }

    template <class datum> datum readData(uint16_t inAddr = 0, boolean advanceIP = true) {
      if (advanceIP) {
        inAddr = _ip16;
        _ip16 += sizeof(datum);
      }



      datum * dptr = reinterpret_cast<datum*>(&_mem[inAddr]);
      datum d = readDataAtPtr<datum>(dptr);
      return d;
    }



    template <class datum>
    void writeData(datum d, uint16_t inAddr = 0, boolean writeAtIP = true, boolean advanceIP = true) {
      if (writeAtIP) {
        inAddr = _ip16;

      }
      if (advanceIP)
        _ip16 += sizeof(datum);
      datum * dptr = reinterpret_cast<datum*>(&_mem[inAddr]);
      *dptr = d;
    }
    /*
        template <class A1 = uint16_t, class A2 = uint8_t, class A3 = uint8_t>
        void writeInstruction(Opcode c, A1 a1 = 0, A2 a2 = 0, A3 a3 = 0)
        {
          writeData(c);
          OpcodeAndDataWidth opPair = getOpcodeAndDataWidth(c);

          // This switch only does something if we have an instruction with an argument
          switch (opPair.c) {
            case Opcode::SP_ADJ: {
                writeData(a2, _ip16);
                break;
              }
            case Opcode::INC_SPREL_UINT_8: {
                writeData(a2, _ip16);
                break;
              }
            case Opcode::MOV_SPREL2_REG_8: {
                writeData(a2, _ip16);
                writeData(a3, _ip16);
                break;
              }
            case Opcode::MOV_REG2_SPREL_8: {
                writeData(a2, _ip16);
                writeData(a3, _ip16);
                break;
              }
            case Opcode::PUSH_CONST_8:
              writeData(a2, _ip16);
              break;
            case Opcode::CMP_INT_8:
              writeData(a2, _ip16);
              break;
            case Opcode::UJMP:
              writeData(a1, _ip16);
              break;
            case Opcode::JEQ:
              writeData(a1, _ip16);
              break;
            case Opcode::CALL:
              writeData(a1, _ip16);
              break;
            case Opcode::PUSH_MEM_8:
              writeData(a1, _ip16);
              break;
            case Opcode::POP_REGS_8:
              writeData(a2, _ip16);
              break;
            case Opcode::ADD_UINT_8:
            case Opcode::MUL_UINT_8:
            case Opcode::DIV_UINT_8:
            case Opcode::SUB_UINT_8:
              writeData(a2, _ip16);
              break;
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
              break;
          }
        }
    */
};

#endif
