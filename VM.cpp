#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"

/* Machine Design Overview:

    Registers: There are 16 32bit general purpose regs. Two are actually "reserved" though they are referenced
    using the standard instructions (i.e. there are no special PUSH or POP instructions for them). R0 is essentially
    the "accumulator" that is the target of all math ops. R1 is the "state register". Bit fields here determine data type,
    data width, perhaps addressing mode.


    Instruction Families

    -- <Math op> <RA:4 bits, RB: 4bits>: ADD, SUB, MUL, DIV, POW, OR, AND, NOT: all use registers only.
      Data type is set using mode register

    -- <Jump> <Addr: 16 bits> <RA:4 bits, RB: 4bits>: JEQ, JNE, JLT, JGT, JGTE, JLTE, JMP: Jump to Addr if registers
      are; equal, not equal, RA < RB, RA > RB, RA >= RB, RA <= RB, any (i.e. unconditional jump).

    -- <Stack op> {<Addr: 16 bits> || <RA:4 bits, RB: 4bits> || No argument} These are the ops;
    PUSHREGS <RA:4 bits, RB: 4bits>: Pushes two registers onto the stack according to current data mode. If RA = RB
    we only push one.

    POPREGS <RA:4 bits, RB: 4bits>: Pops stack values into two registers. If RA = RB we only pop one.
    we only push one.

    PUSH <Addr: 16 bits>: Push the object at <Addr> onto the stack
    POP <Addr: 16 bits>: Pop the stack into memory at <Addr>

    PUSHCONST <variable width: 8 - 32 bits>: Push the specified constant onto the stack. Size of constant is
    determined by mode. This is a convenience op to allow one-time-use constants to be inlined instead
    of storing them somewhere and referencing that address.M


    PUSHALLREGS <No argument: 0 bits>: Push contents of all registers onto the stack. This is a shortcut
    used to make saving of stack frame state convenient when calling a function within a function.

    POPALLREGS <No argument: 0 bits>: Pop contents of all registers from the stack. This is a shortcut
    used to make resotation of stack frame state convenient when returning from a called
    function within a function.

    -- CALL <Addr: 16 bits>: pushes the return address onto the stack. This is computed as the instruction
    directly following CALL.

    -- <


  Example program

  func pow(x,n)
                  SP:STACK_TOP - 2*dm, Stack [x,n]
  PUSH_CONST_88 0:8, [0:dm,x,n], SP:STACK_TOP - 3*dm
  PUSH_CONST_88 1:8, [1:dm,0:dm,x,n], SP:STACK_TOP - 4*dm
  CMP_SPREL 1,3, (STACK_TOP - 1*dm, STACK_TOP - 3*dm)

  JEQ BAILOUT
  INC 1:(SP-1*dm), [1,1:dm,x,n]
  MUL_SPREL 2,0, (SP-2*dm, SP-0*dm --> SP-0*dm) [x,1:dm,x,n]

  Label: BAILOUT
  POPREG 0 (SP --> R0) [n:dm,x,n]
  SP += 3*dm []
  PUSHREG 0 [x^n]
  RET



*/

static const char* VM::_dataModeStrings[8] = {"U8", "U16", "U32", "I8", "I16", "I32", "FL", "STR"};
static const uint8_t VM::dataWidth[] = {1, 2, 4, 1, 2, 4, 4, 2};

void VM::writeString(char * sptr, uint16_t inAddr , boolean advanceIP) {
  uint16_t stringLength =  getStringLength(sptr);
  char *copysptr = sptr;
  if (advanceIP) {
    inAddr = _ip16;
    _ip16 += stringLength;
  }
  for (uint16_t i = 0; i < stringLength; i++ ) {
    _mem[inAddr + i] = *copysptr;
    copysptr++;
  }
}

void VM::changeIP(int16_t addressDelta) {
  // The zero here acts as a semaphor value causing the address to reset to its original value.
  if (addressDelta == 0)
    _ip16 = _ip16Copy;
  else
    _ip16 += addressDelta;
}

void VM::createBinding(uint8_t pin, uint8_t io, boolean ad, uint16_t addr) {
  _pinBindings[pin].setIO(io);
  _pinBindings[pin].setAD(ad);
  _pinBindings[pin].setAddress(addr);
  _pinBindings[pin].setPin(pin);
}

void VM::PinBinding::print() {
  String modeStr = "";
  if (_ad ==  true) {
    modeStr += "A";
  }
  else {
    modeStr += "D";
  }
  switch (_io) {
    case INPUT:
      modeStr += "I";
      break;
    case OUTPUT:
      modeStr += "O";
      break;
    case INPUT_PULLUP:
      modeStr += "P";
      break;
    case NOT_BOUND:
      modeStr = "Unbound";
      break;
  }
  dprintln("Pin " + String(_pin) + ", addr:" + String(_address) + ", mode:" + modeStr);
}

VM::PinBinding::PinBinding() {
  _io = NOT_BOUND; // They all begin life de-activated.
}

void VM::PinBinding::setIO(uint8_t m) {
  pinMode(_pin, m);
  _io = m;
}
uint8_t VM::PinBinding::getIO() {
  return _io;
}

void VM::PinBinding::setAD(boolean ad) {
  _ad = ad;
}

boolean VM::PinBinding::getAD() {
  return _ad;
}

void VM::PinBinding::setPin(uint8_t pin) {
  _pin = pin;
}

void VM::PinBinding::setAddress(uint16_t address) {
  _address = address;
}

uint8_t VM::PinBinding::getPin() {
  return _pin;
}

void VM::PinBinding::updatePin(VM & vm) {
  if (_io == NOT_BOUND) {
    //dprintln("Pin " + String(_pin) + " inactive");
    return;
  }

  if (_ad) {
    switch (_io) {
      case OUTPUT: {
          //dprint("AnalogWrite to pin #: " + String(_pin) + ", ");

          uint8_t val = vm.readData <uint8_t> (_address);
          dprintln("Writing " + String(val) + " to pin " + String(_pin));
          analogWrite(_pin, val);
          //vm.push(valCell);

          //dprintln("leaving AnalogWrite::exec()--");
          break;
        }
      case INPUT:
        {
          uint16_t val = analogRead(_pin);
          dprintln("Got value " + String(val) + " from pin " + String(_pin));
          vm._mem[_address] = val;
          break;
        }
    }
  }
  else {
    // FIXME: need to fill in digital behavior
  }
  // dprintln("");
}

/*
  uint8_t  VM::readMem(uint16_t i) {
  return _mem[i];
  }
*/


void VM::printStatus() {
  String amString = (_am == AddressingMode::REL) ? "Relative" : "Absolute";
  dprintln("IP:" + String(static_cast<uint16_t>(_ip16)) + ", "
           + "SP:" + String(_SP) + ", Data Mode: " + getDataTypeAndWidthString()
           + ", Address Mode: " + amString);
}




void VM::transferData(uint16_t addr, uint8_t * buf, DataMode dm,
                      boolean toStack, boolean adjustSP,
                      boolean alterMemory) {

}

VM::VM(uint16_t memSize, uint16_t stackSize):  _memSize(memSize), _stackSize(stackSize) {

  _dm = DataMode::UINT8;
  _am = AddressingMode::ABS;
  _mem = new uint8_t[memSize];
  _ip16 = 0;
  _SP = STACK_TOP;
  _stackSize = stackSize;
  _ip16Copy = _ip16;
  _AP = 0;

  // Fills the memory and stack with some values for now to show that it's working

  for (uint16_t i = 0; i < memSize; i++)
    _mem[i] = i;

  for (uint8_t i = 0; i < 16; i++)
    _reg[i * 4] = i;
}

void VM::setSP(uint16_t newIP) {
  dprintln("New SP:" + String(newIP));
  _SP = newIP;
}

void VM::setIP(uint16_t newIP) {
  dprintln("New IP:" + String(newIP));
  //_IP = newIP;
  _ip16 = newIP;
}

int16_t VM::getStringLength(char * startAddr) {
  char * p = startAddr;
  uint16_t i = 0;
  //dprintln("VM::getStringLength(char * startAddr)");
  while (true) {
    if (*p == 0 || i >= MAX_STRING_LENGTH )
      return i;
    dprintln("At index " + String(i) + " char is " + String(*p));
    i++;
    p++;

  }

}

//void VM::moveData(uint8_t * srcptr, uint8_t * destptr, DataMode dm) {
void VM::moveData(uint8_t * srcptr, uint8_t * destptr, uint8_t datumWidth) {

  for (uint8_t i = 0; i < datumWidth; i++)
    destptr[i] = srcptr[i];
}

uint8_t * VM::getPtr(uint16_t addr, Location locationType) {


  //, SPREL, MEM_IND, SP_IND,
  switch (locationType) {
    case Location::MEM:
      return &_mem[addr];
      break;
    case Location::SPREL:
      return &_mem[_SP + addr];
      break;
    case Location::MEM_IND:
      addr = _mem[addr];
      return &_mem[addr];
      break;
    case Location::SPREL_IND:
      addr = _mem[_SP + addr];
      return &_mem[addr];
      break;
    case Location::REG:
      return &_reg[addr * 4];
      break;

  }
}

String VM::getDataTypeAndWidthString(DataMode dm) {

  if (dm == DataMode::INVALID_MODE) {
    dm = _dm;
  }
  uint8_t dmuint = static_cast<uint8_t>(dm);
  String sdm = String(_dataModeStrings[dmuint]);

  return sdm;
}

static uint8_t VM::getDataWidth(DataMode dm) {
  return dataWidth[static_cast<uint8_t>(dm)];
}

static Opcode VM::getOpcodeByDataWidth(Opcode c, uint8_t dw) {
  uint8_t cval = static_cast<uint8_t>(c);
  switch (dw) {
    case 1:
      return c;
      break;
    case 2:
      return static_cast<Opcode>(cval + static_cast<uint8_t>(Opcode::END_8));
      break;
    case 4:
      return static_cast<Opcode>(cval + static_cast<uint8_t>(Opcode::END_8) * 2);
      break;
    default:
      return c;
  }

}


static OpcodeAndDataWidth VM::getOpcodeAndDataWidth(Opcode c) {
  uint8_t cval = static_cast<uint8_t>(c);
  const uint8_t widths[] = {1, 2, 4};
  OpcodeAndDataWidth opcodeAndWidth;

  if (cval < static_cast<uint8_t>(Opcode::END_8) * 3) {

    opcodeAndWidth.c = static_cast<Opcode>(cval % static_cast<uint8_t>(Opcode::END_8));
    opcodeAndWidth.dw = widths[cval / static_cast<uint8_t>(Opcode::END_8)];
  }
  else {
    opcodeAndWidth.c = c;
    opcodeAndWidth.dw = 0;
  }

  return opcodeAndWidth;
}

RegPair VM::getRegPair(uint8_t registers) {
  dprintln("POP targets = :" + String(registers));
  RegPair rp;
  rp.reg1 = registers & 0x0f; // low nibble (4bits)
  rp.reg2 = (registers & 0xf0) >> 4; // high nibble (4bits)
  dprintln("reg1 = :" + String(rp.reg1));
  dprintln("reg2 = :" + String(rp.reg2));
  return rp;
}

void VM::exec(Opcode opcode) {
  uint8_t opcodeVal = static_cast<uint8_t>(opcode);
  uint8_t * buf = NULL;

  OpcodeAndDataWidth opPair = VM::getOpcodeAndDataWidth(opcode);
  dprintln("Processed opcode = " + String(static_cast<uint8_t>(opPair.c)));
  dprintln("Data is " + String(opPair.dw) + " bytes wide");
  opcode = opPair.c;
  //uint8_t dw = opPair.dw;
  if (opcode == Opcode::BINDAO || opcode == Opcode::BINDAI ||
      opcode == Opcode::BINDDO || opcode == Opcode::BINDDI ||
      opcode == Opcode::BINDAP || opcode == Opcode::BINDDP) {

    uint16_t addr = readData <uint16_t> ();
    uint8_t pin = readData <uint8_t> ();
    uint8_t io;
    boolean ad;
    switch (opcode) {
      case Opcode::BINDAO:
        ad = true;
        io = OUTPUT;
        break;
      case Opcode::BINDDO:
        ad = false;
        io = OUTPUT;
        break;
      case Opcode::BINDAI:
        ad = true;
        io = INPUT;
        break;
      case Opcode::BINDDI:
        ad = false;
        io = INPUT;
        break;
      case Opcode::BINDAP:
        ad = true;
        io = INPUT_PULLUP;
        break;
      case Opcode::BINDDP:
        ad = false;
        io = INPUT_PULLUP;
        break;
      default:
        break;
    }
    createBinding(pin, io, ad, addr);
  }
  else if (opcode >= Opcode::PP_START_8 && opcode <= Opcode::PP_END_8) {

    uint8_t * srcptr;
    uint8_t * destptr;
    //uint8_t stackAdjustment = VM::getDataWidth(_dm);
    //if (_am == AddressingMode::REL)
    //  addr = readData <uint16_t> (addr, false);
    switch (opcode) {
      case Opcode::PUSH_MEM_8: {
          uint16_t addr = readData <uint16_t> ();
          srcptr = getPtr(addr, Location::MEM);
          dprintln("PUSH from address:" + String(addr));
          _SP -= opPair.dw;
          dprintln("_SP = :" + String(_SP));
          destptr = &_mem[_SP];

          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::POP_REGS_8: {
          uint8_t targetRegisters = readData <uint8_t> ();
          RegPair tr = getRegPair(targetRegisters);
          srcptr = getPtr(_SP, Location::MEM);
          destptr = getPtr(tr.reg1, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          _SP += opPair.dw;
          srcptr = getPtr(_SP, Location::MEM);
          dprintln("POP _SP = :" + String(_SP));
          destptr = getPtr(tr.reg2, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          _SP += opPair.dw;
          dprintln("POP _SP = :" + String(_SP));
          break;
        }
    }


  }
  else if (opcode == Opcode::ADD_INT_8 || opcode == Opcode::SUB_INT_8 ||
           opcode == Opcode::MUL_INT_8 || opcode == Opcode::DIV_INT_8) {

    uint8_t * firstArg = new uint8_t[4];

    uint8_t * secondArg = new uint8_t[4];
    zeros(firstArg, 4);
    zeros(secondArg, 4);
    transferData(0, firstArg, _dm, false, true, false);
    transferData(0, secondArg, _dm, false, true, false);
    // Make 4 byte buffers for all ops
    DataMode generalType = static_cast<DataMode>(static_cast<uint8_t>(_dm) & 0b0011);
    switch (opcode) {
      // To simplify the code for the mathematical instructions I have decided to just compute
      // all types of int and uint as 32 bit types and then stuff them back into
      // the appropriate size containers when done. Eventually we should trap
      // overflows by checking the "high bytes" as needed but this isn't done ATM.
      case Opcode::ADD_INT_8: {
          dprint (F("OPCODE: ADD "));
          //dprintln(getDataTypeAndWidthString());
          switch (generalType) {

            case DataMode::UINT8:
            case DataMode::UINT16:
            case DataMode::UINT32: {

                uint32_t * sum = new uint32_t;
                *sum = *reinterpret_cast<uint32_t*>(firstArg) + *reinterpret_cast<uint32_t*>(secondArg);
                transferData(0, reinterpret_cast<uint8_t*>(sum), _dm, true, true, false);
                delete sum;
                break;
              }
            case DataMode::INT8:
            case DataMode::INT16:
            case DataMode::INT32: {

                int32_t * sum = new int32_t;
                *sum = *reinterpret_cast<int32_t*>(firstArg) + *reinterpret_cast<int32_t*>(secondArg);
                transferData(0, reinterpret_cast<uint8_t*>(sum), _dm, true, true, false);
                delete sum;
                break;
              }
            case DataMode::FLOAT: {

                float * sum = new float;
                *sum = *reinterpret_cast<float*>(firstArg) + *reinterpret_cast<float*>(secondArg);
                transferData(0, reinterpret_cast<uint8_t*>(sum), _dm, true, true, false);
                delete sum;
                break;
              }
            case DataMode::STRING: {
                //FIXME
                break;
              }
          }

        }
      case Opcode::SUB_INT_8:
      case Opcode::MUL_INT_8:
      case Opcode::DIV_INT_8:
        break;

      default:
        break;
    }
    delete firstArg;
    delete secondArg;

  }
  else {
    switch (opcode) {
      /*
        case Opcode::DATA_FLOAT:
        _dm = DataMode::FLOAT;
        dprintln (F("DataMode::FLOAT;"));
        break;
        case Opcode::DATA_UINT8:
        _dm = DataMode::UINT8;
        dprintln (F("DataMode::UINT8;"));
        break;
        case Opcode::DATA_STRING:
        _dm = DataMode::STRING;
        dprintln (F("DataMode::STRING;"));
        break;

        case Opcode::REL_MODE:
        _am = AddressingMode::REL;
        dprintln (F("AddressingMode::REL;"));
        break;

      */
      case Opcode::NOOP:

        dprintln (F("NOOP -- Doing nothing!"));
        break;
      default:
        break;
    }
  }
  // FIXME: Do the math here
}


void VM::step() {
  updateBoundData();
  printStatus();
  dprint(" / ");

  Opcode opcode = readData <Opcode> (_ip16);
  dprintln("Opcode:" + String(static_cast<uint8_t>(opcode)));
  exec(opcode);

}

void VM::updateBoundData() {
  for (uint8_t i = 0; i < NUM_PINS; i++) {
    //dprint("i:" + String(i));
    _pinBindings[i].updatePin(*this);

  }
}

void VM::printBindings() {
  for (uint8_t i = 0; i < NUM_PINS; i++)
    _pinBindings[i].print();
}



void VM::printMem(uint16_t startAddr, uint16_t endAddr) {
  dprintln(repeatString("M", 20));
  for (uint16_t i = startAddr; i < endAddr; i++)
    dprintln(String(i) + ": " + String(static_cast<uint8_t>(_mem[i])));
}

void VM::printStack() {
  dprintln(repeatString("S", 20));
  for (uint16_t i = STACK_TOP - 1; i > STACK_TOP - _stackSize; i--) {
    dprintln(String(i) + ": " + String(static_cast<uint8_t>(_mem[i])));
  }
}

void VM::printRegisters() {
  dprintln(repeatString("R", 20));
  for (uint8_t i = 0; i < 16 ; i++) {
    uint32_t * regptr32 = reinterpret_cast<uint32_t*>(&_reg[i * 4]);
    dprintln("Reg " + String(i) + ": " + String(*regptr32));
  }
}

void VM::reset() {
  _ip16 = 0;
  _SP = 0;
}

