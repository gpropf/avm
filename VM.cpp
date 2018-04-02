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
    of storing them somewhere and referencing that address.

    PUSHALLREGS <No argument: 0 bits>: Push contents of all registers onto the stack. This is a shortcut
    used to make saving of stack frame state convenient when calling a function within a function.

    POPALLREGS <No argument: 0 bits>: Pop contents of all registers from the stack. This is a shortcut
    used to make resotation of stack frame state convenient when returning from a called
    function within a function.

    -- CALL <Addr: 16 bits>: pushes the return address onto the stack and jumps to function address.
    This is computed as the instruction directly following CALL.

  // ==============================================================
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
    return;
  }

  if (_ad) {
    switch (_io) {
      case OUTPUT: {
          uint16_t val = vm.readData <uint16_t> (_address);
          dprintln("Writing " + String(val) + " to pin " + String(_pin));
          analogWrite(_pin, val);
          break;
        }
      case INPUT:
        {
          uint16_t val = analogRead(_pin);
          dprintln("Got value " + String(val) + " from pin " + String(_pin));
          //uint16_t * u16ptr = reinterpret_cast<uint16_t>(&vm._mem[_address]);
          vm.writeData<uint16_t>(val, _address, false, false);
          //vm._mem[_address] = val;
          //*u16ptr = val;
          break;
        }
    }
  }
  else {
    // FIXME: need to fill in digital behavior
  }
}

void VM::printStatus() {
  //String amString = (_am == AddressingMode::REL) ? "Relative" : "Absolute";
  dprintln("IP:" + String(static_cast<uint16_t>(_ip16)) + ", "
           + "SP:" + String(_SP) + " _cmpReg: " + String(static_cast<uint16_t>(_cmpReg)));
}

VM::VM(uint16_t memSize, uint16_t stackSize):  _memSize(memSize), _stackSize(stackSize) {

  //_dm = DataMode::UINT8;
  //_am = AddressingMode::ABS;
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
  dprintln("POP targets = :" + String(registers),2);
  RegPair rp;
  rp.reg1 = registers & 0x0f; // low nibble (4bits)
  rp.reg2 = (registers & 0xf0) >> 4; // high nibble (4bits)
  dprintln("reg1 = :" + String(rp.reg1),2);
  dprintln("reg2 = :" + String(rp.reg2),2);
  return rp;
}

void VM::exec(Opcode opcode) {
  dprintln("Before inst (sp,ip) : (" + String(_SP) + "," + String(_ip16) + ")");
  uint8_t opcodeVal = static_cast<uint8_t>(opcode);
  uint8_t * buf = NULL;

  OpcodeAndDataWidth opPair = VM::getOpcodeAndDataWidth(opcode);
  dprint("Opcode : " + String(static_cast<uint8_t>(opPair.c)));
  dprintln(", Data width : " + String(opPair.dw));
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
       dprintln(F("BINDAO"), 1);
        ad = true;
        io = OUTPUT;
        break;
      case Opcode::BINDDO:
      dprintln(F("BINDDO"), 1);
        ad = false;
        io = OUTPUT;
        break;
      case Opcode::BINDAI:
      dprintln(F("BINDAI"), 1);
        ad = true;
        io = INPUT;
        break;
      case Opcode::BINDDI:
      dprintln(F("BINDDI"), 1);
        ad = false;
        io = INPUT;
        break;
      case Opcode::BINDAP:
      dprintln(F("BINDAP"), 1);
        ad = true;
        io = INPUT_PULLUP;
        break;
      case Opcode::BINDDP:
      dprintln(F("BINDDP"), 1);
        ad = false;
        io = INPUT_PULLUP;
        break;
      default:
        break;
    }
    createBinding(pin, io, ad, addr);
  }
  else if (opcode < Opcode::FIXED_WIDTH_BASE) {

    uint8_t * srcptr;
    uint8_t * destptr;
    switch (opcode) {
      case Opcode::INC_SPREL_UINT_8: {
          dprintln(F("INC_SPREL_UINT_8"), 1);
          uint8_t sprel = readData <uint8_t> ();
          srcptr = getPtr(sprel, Location::SPREL);
          (*srcptr)++;
          break;
        }

      case Opcode::CMP_INT_8: {
          dprintln(F("CMP_INT_8"), 1);
          uint8_t targetRegisters = readData <uint8_t> ();
          RegPair rp = getRegPair(targetRegisters);
          switch (opPair.dw) {
            case 1: {
                int8_t r1;
                int8_t r2;
                castRegData(r1, r2, rp);
                if (r1 < r2) {
                  dprintln("R1 < R2: " + String(r1) + "," + String(r2), 2);
                  _cmpReg = Comparison::LESS_THAN;
                }
                else if (r1 > r2) {
                  dprintln("R1 > R2: " + String(r1) + "," + String(r2), 2);
                  _cmpReg = Comparison::GREATER_THAN;
                }
                else {
                  dprintln("R1 = R2: " + String(r1) + "," + String(r2), 2);
                  _cmpReg = Comparison::EQUAL;
                }
                break;
              }
            case 2: {
                break;
              }
            case 4: {
                break;
              }

          }
          break;
        }
      case Opcode::MOV_SPREL2_REG_8: {
          dprintln(F("MOV_SPREL2_REG_8"), 1);
          uint8_t sprel = readData <uint8_t> ();
          uint8_t reg = readData <uint8_t> ();
          srcptr = getPtr(sprel, Location::SPREL);
          destptr = getPtr(reg, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::MOV_REG2_SPREL_8: {
          dprintln(F("MOV_REG2_SPREL_8"), 1);
          uint8_t reg = readData <uint8_t> ();
          uint8_t sprel = readData <uint8_t> ();
          destptr = getPtr(sprel, Location::SPREL);
          srcptr = getPtr(reg, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::PUSH_CONST_8: {
          dprintln(F("PUSH_CONST_8"), 1);
          srcptr = getPtr(_ip16, Location::MEM);
          _ip16++;
          _SP -= opPair.dw;
          destptr = &_mem[_SP];
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::PUSH_MEM_8: {
          dprintln(F("PUSH_MEM_8"), 1);
          uint16_t addr = readData <uint16_t> ();
          srcptr = getPtr(addr, Location::MEM);
          _SP -= opPair.dw;
          destptr = &_mem[_SP];
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::POP_REGS_8: {
          dprintln(F("POP_REGS_8"), 1);
          uint8_t targetRegisters = readData <uint8_t> ();
          RegPair tr = getRegPair(targetRegisters);
          srcptr = getPtr(_SP, Location::MEM);
          destptr = getPtr(tr.reg1, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          _SP += opPair.dw;
          srcptr = getPtr(_SP, Location::MEM);
          destptr = getPtr(tr.reg2, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          _SP += opPair.dw;
          break;
        }
      case Opcode::ADD_INT_8: {
          dprintln(F("ADD_INT_8"), 1);
          uint8_t targetRegisters = readData <uint8_t> ();
          RegPair tr = getRegPair(targetRegisters);
          srcptr = getPtr(tr.reg1, Location::REG);
          destptr = getPtr(tr.reg2, Location::REG);
          uint32_t * srcreg = reinterpret_cast<uint32_t*>(srcptr);
          uint32_t * destreg = reinterpret_cast<uint32_t*>(destptr);
          *destreg += *srcreg;
          dprintln("Sum of register pair (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg), 2);
          break;
        }
      case Opcode::MUL_INT_8: {
          dprintln(F("MUL_INT_8"), 1);
          uint8_t targetRegisters = readData <uint8_t> ();
          RegPair tr = getRegPair(targetRegisters);
          srcptr = getPtr(tr.reg1, Location::REG);
          destptr = getPtr(tr.reg2, Location::REG);
          uint32_t * srcreg = reinterpret_cast<uint32_t*>(srcptr);
          uint32_t * destreg = reinterpret_cast<uint32_t*>(destptr);
          *destreg *= *srcreg;
          dprintln("Product of register pair (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg), 2);
          break;
        }       
    }
  }
  else {
    
    // Above FIXED_WIDTH_BASE instructions
    
    switch (opcode) {
      case Opcode::SP_ADJ: {
          dprintln(F("SP_ADJ"), 1);
          uint8_t adjustment = readData <uint8_t> ();
          _ip16 += adjustment;
          break;
        }
      case Opcode::UJMP: {
          dprintln(F("UJMP"), 1);
          uint16_t addr = readData <uint16_t> ();
          _ip16 = addr;
          dprintln("Unconditional jump to addr: " + String(addr));
          break;
        }
      case Opcode::JEQ: {
          dprintln(F("JEQ"), 1);
          uint16_t addr = readData <uint16_t> ();
          if (_cmpReg == Comparison::EQUAL ) {
            _ip16 = addr;
            dprintln("Jumping to addr: " + String(addr));
          } else {
            dprintln("Comparison Not Equal - no jump to addr: " + String(addr));
          }
          break;
        }
      case Opcode::CALL: {
          uint16_t addr = readData <uint16_t> ();
          uint8_t * ipValPtr = reinterpret_cast<uint8_t*>(&_ip16);
          _SP -= 2; // make room on stack for return address
          uint8_t * destptr = &_mem[_SP];
          dprintln("CALL: " + String(addr), 1);
          moveData(ipValPtr, destptr, 2);
          _ip16 = addr;
          break;
        }
      case Opcode::RET: {
          uint16_t * retAddr = reinterpret_cast<uint16_t*>(&_mem[_SP]);
          dprintln("RET to addr: " + String(*retAddr));
          _SP += 2;
          _ip16 = *retAddr;
          break;
        }
      case Opcode::NOOP:
        dprintln (F("NOOP"), 1);
        break;
      default:
        break;
    }
  }
   dprintln("After inst (sp,ip) : (" + String(_SP) + "," + String(_ip16) + ")\n");
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

