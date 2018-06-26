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

String const VM::_dataModeStrings[] = {String((char *)"u8"), String((char *)"u16"), String((char *)"u32"),
                                       String((char *)"i8"), String((char *)"i16"), String((char *)"i32"),
                                       String((char *)"f"), String((char *)"s")
                                      };
const uint8_t VM::dataWidth[] = {1, 2, 4, 1, 2, 4, 4, 2};


String VM::getAsString(uint16_t addr, const String modeString) const {
  uint8_t numModeStrings = sizeof(VM::_dataModeStrings) / sizeof(VM::_dataModeStrings[0]);
  //dprintln(F("sizeof(VM::_dataModeStrings): ") + String(numModeStrings));
  for (uint8_t i = 0; i < numModeStrings; i++) {
    if (modeString == VM::_dataModeStrings[i]) {
      return getAsString(addr, static_cast<DataMode>(i));
    }
  }
  return String();
}

String VM::getAsString(uint8_t* addr8, const DataMode dm) const {
  //dprintln(F("getAsString: ") + String(static_cast<uint8_t>(dm)), static_cast<uint8_t>(PrintCategory::STATUS));
  switch (dm) {
    case DataMode::UINT8: {
        return String(*reinterpret_cast<uint8_t*>(addr8));
      }
    case DataMode::UINT16: {
        return String(*reinterpret_cast<uint16_t*>(addr8));
      }
    case DataMode::UINT32: {
        return String(*reinterpret_cast<uint32_t*>(addr8));
        //return String(readData<uint32_t>(addr8, false));
      }
    case DataMode::INT8: {
        return String(*reinterpret_cast<int8_t*>(addr8));
        //return String(readData<int8_t>(addr8, false));
      }
    case DataMode::INT16: {
        return String(*reinterpret_cast<int16_t*>(addr8));
        //return String(readData<int16_t>(addr8, false));
      }
    case DataMode::INT32: {
        return String(*reinterpret_cast<int32_t*>(addr8));
        //return String(readData<int32_t>(addr8, false));
      }
    case DataMode::FLOAT: {
        return String(*reinterpret_cast<float*>(addr8));
        //  return String(readData<float>(addr8, false));
      }
    case DataMode::STRING: {
        /*
            This one is different. We interpret the value at addr8 as a uint16_t
            VM memory address so if the thing at addr8 is 84 then we actually look
            at _mem[84] for the start of the null-terminated string.
        */

        uint16_t memAddr = *reinterpret_cast<uint16_t*>(addr8);
        //dprintln(F("String addr = ") + String(memAddr), static_cast<uint8_t>(PrintCategory::STATUS));
        char currentChar = 39;
        // 39 is the code for "'". It's a cute way to have currentChar be non-zero
        // at the start and as a way to start the single-quoted string.
        String s = String((char *)"");
        boolean firstChar = true;
        // dprintln(F("memAddr:") + String(memAddr), static_cast<uint8_t>(PrintCategory::REPL));
        while (memAddr < VM_MEM_SIZE && currentChar != 0) {
          if (!firstChar && currentChar != 0)
            s += currentChar;
          firstChar = false;
          currentChar = readDataConst<char>(memAddr++);
        }
        // dprintln(F("STRING:") + s, static_cast<uint8_t>(PrintCategory::REPL));

        return s;
      }
    default:
      return String((char *)"");
  }
  return String((char *)"");
}

String VM::getAsString(uint16_t addr, const DataMode dm) const {

  uint8_t * realAddrPtr = reinterpret_cast<uint8_t*>(&_mem[addr]);
  return getAsString(realAddrPtr, dm);
  /*
    switch (dm) {
    case DataMode::UINT8: {
        return String(readData<uint8_t>(addr, false));


      }
    case DataMode::UINT16: {
        return String(readData<uint16_t>(addr, false));
      }

    case DataMode::UINT32: {
        return String(readData<uint32_t>(addr, false));
      }
    case DataMode::INT8: {
        return String(readData<int8_t>(addr, false));
      }
    case DataMode::INT16: {
        return String(readData<int16_t>(addr, false));
      }
    case DataMode::INT32: {
        return String(readData<int32_t>(addr, false));
      }

    case DataMode::FLOAT: {
        return String(readData<float>(addr, false));
      }

    case DataMode::STRING: {

        char currentChar = 39;
        // 39 is the code for "'". It's a cute way to have currentChar be non-zero
        // at the start and as a way to start the single-quoted string.
        String s = F("");
        while (addr < VM_MEM_SIZE && currentChar != 0) {
          s += currentChar;
          currentChar = readData<char>(addr++, false);

        }

        return s + "'";

      }
    default:
      return "";

    }
  */
  //return "";
}


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
  String modeStr = String((char *)"");
  if (_ad ==  true) {
    modeStr += String((char *)"A");
  }
  else {
    modeStr += String((char *)"D");
  }
  switch (_io) {
    case INPUT:
      modeStr += String((char *)"I");
      break;
    case OUTPUT:
      modeStr += String((char *)"O");
      break;
    case INPUT_PULLUP:
      modeStr += String((char *)"P");
      break;
    case NOT_BOUND:
      modeStr = String((char *)"Unbound");
      break;
  }
  dprint(F("Pin "));
  dprint(String(_pin));
  dprint(F(", addr:"));
  dprint(String(_address));
  dprint(F(", mode:"));
  dprintln(modeStr);
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
          uint8_t val = vm.readData <uint8_t> (_address, false);
          // We MUST set the flag that advances the IP to false if reading from a
          // specific address. This probably can be coded as policy into readDate()
          // without problems but that needs to be tested.
          dprint(F("Writing Analog Value: "));
          dprint(String(val));
          dprint(F(" to pin "));
          dprintln(String(_pin));
          analogWrite(_pin, val);

          break;
        }
      case INPUT:
        {
          uint16_t val = analogRead(_pin);
          dprint(F("Got analog value "));
          dprint(String(val));
          dprint(F(" from pin "));
          dprintln(String(_pin));
          //uint16_t * u16ptr = reinterpret_cast<uint16_t>(&vm._mem[_address]);
          vm.writeData<uint16_t>(val, _address, false, false);
          //vm._mem[_address] = val;
          //*u16ptr = val;
          break;
        }
    }
  }
  else {
    // Digital IO
    switch (_io) {
      case OUTPUT: {
          uint16_t val = vm.readData <uint16_t> (_address, false);
          dprint(F("Writing Digital Value"));
          dprint(String(val));
          dprint(F(" to pin "));
          dprintln(String(_pin));
          digitalWrite(_pin, val);
          break;
        }
      case INPUT:
        {
          uint16_t val = digitalRead(_pin);
          dprintln(F("Got digital value "));
          dprint(String(val));
          dprint(F(" from pin "));
          dprintln(String(_pin));
          //uint16_t * u16ptr = reinterpret_cast<uint16_t>(&vm._mem[_address]);
          vm.writeData<uint16_t>(val, _address, false, false);
          //vm._mem[_address] = val;
          //*u16ptr = val;
          break;
        }
    }
  }

}

void VM::printStatus() {
  //String amString = (_am == AddressingMode::REL) ? "Relative" : "Absolute";
  dprint(F("IP:"));
  dprint(String(static_cast<uint16_t>(_ip16)));
  dprint(F(", "));
  dprint(F("SP:"));
  dprint(String(_SP));
  dprint(F(" cmpReg: "));
  dprintln(String(static_cast<uint16_t>(_cmpReg)));
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
    _mem[i] = static_cast<uint8_t>(Opcode::NOOP_INIT);

  for (uint8_t i = 0; i < 16; i++)
    _reg[i * 4] = 0;
}

void VM::setSP(uint16_t newIP) {
  dprint(F("New SP:"));
  dprintln(String(newIP));
  _SP = newIP;
}

int16_t VM::getStringLength(char * startAddr) {
  char * p = startAddr;
  uint16_t i = 0;
  //dprintln(F("VM::getStringLength(char * startAddr)"));
  while (true) {
    if (*p == 0 || i >= MAX_STRING_LENGTH )
      return i;
    dprint(F("At index "));
    dprint(String(i));
    dprint(F(" char is "));
    dprintln(String(*p));
    i++;
    p++;
  }
}

//void VM::moveData(uint8_t * srcptr, uint8_t * destptr, DataMode dm) {
void VM::moveData(uint8_t * srcptr, uint8_t * destptr, uint8_t datumWidth) {
  for (uint8_t i = 0; i < datumWidth; i++)
    destptr[i] = srcptr[i];

}

void VM::loadRegWithConst(uint8_t reg, uint32_t c) {
  /*  Zero the register first to clear out junk from high bytes
      that might be left over from previous use. If we don't do this
      things like comparison and math ops on 8 bit data might malfunction
      since I'm probably going to do those in 32 bits in practice to avoid writing
      everything in triplicate (1,2,4 byte operations requiring different pointer
      type).
  */

  //reg = reg * 4;
  //uint32_t * reg32ptr = reinterpret_cast<uint32_t*>(&_reg);
  //reg32ptr = c;
  uint8_t * cPtr = reinterpret_cast<uint8_t*>(&c);
  for (uint8_t i = 0; i < 4; i++) {

    uint8_t * regbytePtr = &_reg[reg * 4 + i];
    *regbytePtr = *cPtr++;
  }
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
    default:
      return 0;
  }
}

Opcode VM::getOpcodeByDataWidth(Opcode c, uint8_t dw) {
  /* This is the inverse of the VM::getOpcodeAndDataWidth(Opcode c) method */
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

OpcodeAndDataWidth VM::getOpcodeAndDataWidth(Opcode c) {
  /* This function translates a raw opcode into its equivalent
      8 bit code and a data width value in bytes. The data width value ("dw")
      is used to determine which actual operation to perfom.
  */
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

RegPair VM::getRegPair() {
  uint8_t registers = readData <uint8_t> ();
  dprint(F("POP targets = :"), static_cast<uint8_t>(PrintCategory::POP) & static_cast<uint8_t>(PrintCategory::REG));
  dprintln(String(registers), static_cast<uint8_t>(PrintCategory::POP) & static_cast<uint8_t>(PrintCategory::REG));
  RegPair rp;
  rp.reg1 = registers & 0x0f; // low nibble (4bits)
  rp.reg2 = (registers & 0xf0) >> 4; // high nibble (4bits)
  dprint(F("reg1 = :"), static_cast<uint8_t>(PrintCategory::POP) & static_cast<uint8_t>(PrintCategory::REG));
  dprint(String(rp.reg1), static_cast<uint8_t>(PrintCategory::POP) & static_cast<uint8_t>(PrintCategory::REG));
  dprint(F("reg2 = :"), static_cast<uint8_t>(PrintCategory::POP) & static_cast<uint8_t>(PrintCategory::REG));
  dprintln(String(rp.reg2), static_cast<uint8_t>(PrintCategory::POP) & static_cast<uint8_t>(PrintCategory::REG));
  return rp;
}



String VM::OpcodeWithWidth2String(OpcodeAndDataWidth opdw) {
  return String(opdw.dw * 8);
}


void VM::exec(Opcode opcode) {
  //dprintln(F("sp: " + String(_SP) + ",ip: ") + String(_ip16), static_cast<uint8_t>(PrintCategory::REPL));
  //uint8_t opcodeVal = static_cast<uint8_t>(opcode);
  //uint8_t * buf = NULL;

  OpcodeAndDataWidth opPair = VM::getOpcodeAndDataWidth(opcode);
  dprint(F("Opcode: "), static_cast<uint8_t>(PrintCategory::STATUS));

  dprint(String(static_cast<uint8_t>(opPair.c)), static_cast<uint8_t>(PrintCategory::STATUS));
  dprint(F(", Data width : "), static_cast<uint8_t>(PrintCategory::STATUS));
  dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
  opcode = opPair.c;
  //uint8_t dw = opPair.dw;
  if (opcode == Opcode::BINDAO || opcode == Opcode::BINDAI ||
      opcode == Opcode::BINDDO || opcode == Opcode::BINDDI ||
      opcode == Opcode::BINDAP || opcode == Opcode::BINDDP) {

    uint16_t addr = readData <uint16_t> ();
    uint8_t pin = readData <uint8_t> ();
    uint8_t io = INPUT;
    boolean ad = false;
    switch (opcode) {
      case Opcode::BINDAO:
        dprintln(F("BINDAO"), static_cast<uint8_t>(PrintCategory::STATUS));
        ad = true;
        io = OUTPUT;
        break;
      case Opcode::BINDDO:
        dprintln(F("BINDDO"), static_cast<uint8_t>(PrintCategory::STATUS));
        ad = false;
        io = OUTPUT;
        break;
      case Opcode::BINDAI:
        dprintln(F("BINDAI"), static_cast<uint8_t>(PrintCategory::STATUS));
        ad = true;
        io = INPUT;
        break;
      case Opcode::BINDDI:
        dprintln(F("BINDDI"), static_cast<uint8_t>(PrintCategory::STATUS));
        ad = false;
        io = INPUT;
        break;
      case Opcode::BINDAP:
        dprintln(F("BINDAP"), static_cast<uint8_t>(PrintCategory::STATUS));
        ad = true;
        io = INPUT_PULLUP;
        break;
      case Opcode::BINDDP:
        dprintln(F("BINDDP"), static_cast<uint8_t>(PrintCategory::STATUS));
        ad = false;
        io = INPUT_PULLUP;
        break;
      default:
        break;
    }
    createBinding(pin, io, ad, addr);
  }
  else if (opcode < Opcode::FIXED_WIDTH_BASE) {
    /* These instructions mostly end in "_8". This implies that their data substrate is 8 bit.
        However, the code is written so that these instructions are repeated in 16 and 32 bit forms
        directly above the 8 bit ones in numeric order in the instruction set (as interpreted as unsigned
        8 bit ints). The code can distiguish these instructions and apply the operation to the correct
        data width. This makes use of the special OpcodeAndDataWidth data structure and the function
        VM::getOpcodeAndDataWidth(opcode). This function translates a raw opcode into its equivalent
        8 bit code and a data width value in bytes. The data width value ("dw") is used to determine which
        actual operation to perfom.

    */

    // dprintln(F("BELOW FIXED WIDTH BASE: IP = ") + String(_ip16), static_cast<uint8_t>(PrintCategory::STATUS));
    uint8_t * srcptr;
    uint8_t * destptr;
    switch (opcode) {
      case Opcode::INC_SPREL_UINT_8: {
          dprint(F("INC_SPREL_UINT:"), static_cast<uint8_t>(PrintCategory::STATUS)
                 | static_cast<uint8_t>(PrintCategory::MATH));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          uint8_t sprel = readData <uint8_t> ();
          srcptr = getPtr(sprel, Location::SPREL);
          (*srcptr)++;
          break;
        }

      case Opcode::CMP_INT_8: {
          dprint(F("CMP_INT:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          //   uint8_t targetRegisters = readData <uint8_t> ();
          RegPair rp = getRegPair();

          /*
             To save SRAM I got rid of the "correct code" in the commented out
             switch block and do all comparisons with 32 bit
             data.
          */

          int32_t r1;
          int32_t r2;
          castRegData(r1, r2, rp);
          if (r1 < r2) {
            dprint(F("R1 < R2: "), static_cast<uint8_t>(PrintCategory::MATH));

            _cmpReg = Comparison::LESS_THAN;
          }
          else if (r1 > r2) {
            dprint(F("R1 > R2: "), static_cast<uint8_t>(PrintCategory::MATH));
            _cmpReg = Comparison::GREATER_THAN;
          }
          else {
            dprint(F("R1 = R2: "), static_cast<uint8_t>(PrintCategory::MATH));
            _cmpReg = Comparison::EQUAL;
          }
          dprint(String(r1), static_cast<uint8_t>(PrintCategory::MATH));
          dprint(F(","), static_cast<uint8_t>(PrintCategory::MATH));
          dprintln(String(r2), static_cast<uint8_t>(PrintCategory::MATH));
          break;
        }
      case Opcode::MOV_SPREL2_REG_8: {
          dprint(F("MOV_SPREL2_REG:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          uint8_t sprel = readData <uint8_t> ();
          uint8_t reg = readData <uint8_t> ();
          /*  Zero the register first to clear out junk from high bytes
              that might be left over from previous use. If we don't do this
              things like comparison and math ops on 8 bit data might malfunction
              since I'm probably going to do those in 32 bits in practice to avoid writing
              everything in triplicate (1,2,4 byte operations requiring different pointer
              type).
          */
          loadRegWithConst(reg);
          srcptr = getPtr(sprel, Location::SPREL);
          destptr = getPtr(reg, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::MOV_MEM2_REG_8: {
          dprint(F("MOV_MEM2_REG:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          uint16_t addr = readData <uint16_t> ();
          uint8_t reg = readData <uint8_t> ();
          loadRegWithConst(reg);
          srcptr = getPtr(addr, Location::MEM);
          destptr = getPtr(reg, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::MOV_REG2_SPREL_8: {
          dprint(F("MOV_REG2_SPREL:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          uint8_t reg = readData <uint8_t> ();
          uint8_t sprel = readData <uint8_t> ();
          destptr = getPtr(sprel, Location::SPREL);
          srcptr = getPtr(reg, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::MOV_REG2_MEM_8: {
          dprint(F("MOV_REG2_MEM:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          uint8_t reg = readData <uint8_t> ();
          uint16_t mem = readData <uint16_t> ();
          destptr = getPtr(mem, Location::MEM);
          srcptr = getPtr(reg, Location::REG);
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::PUSH_CONST_8: {
          dprint(F("PUSH_CONST:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          srcptr = getPtr(_ip16, Location::MEM);
          _ip16 += opPair.dw;
          // This one's a little different as the arg is read from the next location at the IP
          // and the data width can vary.
          _SP -= opPair.dw;
          destptr = &_mem[_SP];
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::PUSH_MEM_8: {
          dprint(F("PUSH_MEM:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          uint16_t addr = readData <uint16_t> ();
          srcptr = getPtr(addr, Location::MEM);
          _SP -= opPair.dw;
          destptr = &_mem[_SP];
          moveData(srcptr, destptr, opPair.dw);
          break;
        }
      case Opcode::POP_REGS_8: {
          dprint(F("POP_REGS:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          //  uint8_t targetRegisters = readData <uint8_t> ();
          RegPair tr = getRegPair();

          srcptr = getPtr(_SP, Location::MEM);
          destptr = getPtr(tr.reg1, Location::REG);
          moveData(srcptr, destptr, opPair.dw);

          if (tr.reg1 != tr.reg2) {
            _SP += opPair.dw;
            srcptr = getPtr(_SP, Location::MEM);
            destptr = getPtr(tr.reg2, Location::REG);
            moveData(srcptr, destptr, opPair.dw);

          }
          _SP += opPair.dw;
          break;
        }
      case Opcode::ADD_UINT_8: {
          // Add srcreg to destreg and leave the result in destreg
          dprint(F("ADD_UINT:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          //   uint8_t targetRegisters = readData <uint8_t> ();

          /*
            RegPair tr = getRegPair();
            srcptr = getPtr(tr.reg1, Location::REG);
            destptr = getPtr(tr.reg2, Location::REG);
            uint32_t * srcreg = reinterpret_cast<uint32_t*>(srcptr);
            uint32_t * destreg = reinterpret_cast<uint32_t*>(destptr);
          */

          doublePointer<uint32_t> dp = getRegPair2<uint32_t>();

          *(dp.destreg) += *(dp.srcreg);
          /*
            dprintln("Sum of register pair (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                   static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));
          */
          break;
        }
      case Opcode::MUL_UINT_8: {
          // Multiply srcreg and destreg and leave the result in destreg
          dprint(F("MUL_UINT:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          /*
            //  uint8_t targetRegisters = readData <uint8_t> ();
            RegPair tr = getRegPair();
            srcptr = getPtr(tr.reg1, Location::REG);
            destptr = getPtr(tr.reg2, Location::REG);
            uint32_t * srcreg = reinterpret_cast<uint32_t*>(srcptr);
            uint32_t * destreg = reinterpret_cast<uint32_t*>(destptr);
            destreg *= *srcreg;
            dprintln("Product of register pair (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                   static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));

          */
          doublePointer<uint32_t> dp = getRegPair2<uint32_t>();

          *(dp.destreg) *= *(dp.srcreg);
          break;
        }
      case Opcode::SUB_UINT_8: {
          // Substract srcreg from destreg and leave the result in destreg
          dprint(F("SUB_UINT:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          /*
            // uint8_t targetRegisters = readData <uint8_t> ();
            RegPair tr = getRegPair();
            srcptr = getPtr(tr.reg1, Location::REG);
            destptr = getPtr(tr.reg2, Location::REG);
            uint32_t * srcreg = reinterpret_cast<uint32_t*>(srcptr);
            uint32_t * destreg = reinterpret_cast<uint32_t*>(destptr);
            destreg -= *srcreg;
            dprintln("Reg2 - Reg1 (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                   static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));
          */
          doublePointer<uint32_t> dp = getRegPair2<uint32_t>();

          *(dp.destreg) -= *(dp.srcreg);
          break;
        }
      case Opcode::DIV_UINT_8: {
          /*
             Divide destreg by srcreg and leave the result in destreg. This
             allows for repeated division without reloading the srcreg.
             Example: Load srcreg with 2 and destreg with 16, divide once and you
             have srcreg = 2, destreg = 8. You can now divide again by 2 without changing
             the 2. If the operation worked the other way you would load srcreg with 16,
             and destreg with 2. Dividing now leaves you with srcreg = 16, destreg = 8.
             The system is thus not set up to divide again by the same value but its cofactor.
          */
          dprint(F("DIV_UINT:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          /*
            // uint8_t targetRegisters = readData <uint8_t> ();
            RegPair tr = getRegPair();
            srcptr = getPtr(tr.reg1, Location::REG);
            destptr = getPtr(tr.reg2, Location::REG);
            uint32_t * srcreg = reinterpret_cast<uint32_t*>(srcptr);
            uint32_t * destreg = reinterpret_cast<uint32_t*>(destptr);
            destreg /= *srcreg;
            dprintln("Reg2 / Reg1 (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                   static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));
          */
          doublePointer<uint32_t> dp = getRegPair2<uint32_t>();

          *(dp.destreg) /= *(dp.srcreg);
          break;
        }
      case Opcode::ADD_INT_8: {
          // Add srcreg to destreg and leave the result in destreg
          dprint(F("ADD_INT:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          /*
            //  uint8_t targetRegisters = readData <uint8_t> ();
            RegPair tr = getRegPair();
            srcptr = getPtr(tr.reg1, Location::REG);
            destptr = getPtr(tr.reg2, Location::REG);
            int32_t * srcreg = reinterpret_cast<int32_t*>(srcptr);
            int32_t * destreg = reinterpret_cast<int32_t*>(destptr);
            destreg += *srcreg;
            dprintln("Sum of register pair (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                   static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));

          */
          doublePointer<int32_t> dp = getRegPair2<int32_t>();

          *(dp.destreg) += *(dp.srcreg);
          break;
        }
      case Opcode::MUL_INT_8: {
          // Multiply srcreg and destreg and leave the result in destreg
          dprint(F("MUL_INT:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          /*
            //   uint8_t targetRegisters = readData <uint8_t> ();
            RegPair tr = getRegPair();
            srcptr = getPtr(tr.reg1, Location::REG);
            destptr = getPtr(tr.reg2, Location::REG);
            int32_t * srcreg = reinterpret_cast<int32_t*>(srcptr);
            int32_t * destreg = reinterpret_cast<int32_t*>(destptr);
            destreg *= *srcreg;
            dprintln("Product of register pair (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                   static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));
          */
          doublePointer<int32_t> dp = getRegPair2<int32_t>();

          *(dp.destreg) *= *(dp.srcreg);
          break;
        }
      case Opcode::SUB_INT_8: {
          // Substract srcreg from destreg and leave the result in destreg
          dprint(F("SUB_INT:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          /*
            //  uint8_t targetRegisters = readData <uint8_t> ();
            RegPair tr = getRegPair();
            srcptr = getPtr(tr.reg1, Location::REG);
            destptr = getPtr(tr.reg2, Location::REG);
            int32_t * srcreg = reinterpret_cast<int32_t*>(srcptr);
            int32_t * destreg = reinterpret_cast<int32_t*>(destptr);
            destreg -= *srcreg;
            dprintln("Reg2 - Reg1 (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                   static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));
          */
          doublePointer<int32_t> dp = getRegPair2<int32_t>();

          *(dp.destreg) -= *(dp.srcreg);

          break;
        }
      case Opcode::DIV_INT_8: {
          /*
             Divide destreg by srcreg and leave the result in destreg. This
             allows for repeated division without reloading the srcreg.
             Example: Load srcreg with 2 and destreg with 16, divide once and you
             have srcreg = 2, destreg = 8. You can now divide again by 2 without changing
             the 2. If the operation worked the other way you would load srcreg with 16,
             and destreg with 2. Dividing now leaves you with srcreg = 16, destreg = 8.
             The system is thus not set up to divide again by the same value but its cofactor.
          */
          dprint(F("DIV_INT:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          /*
            //   uint8_t targetRegisters = readData <uint8_t> ();
            RegPair tr = getRegPair();
            srcptr = getPtr(tr.reg1, Location::REG);
            destptr = getPtr(tr.reg2, Location::REG);
            int32_t * srcreg = reinterpret_cast<int32_t*>(srcptr);
            int32_t * destreg = reinterpret_cast<int32_t*>(destptr);
            destreg /= *srcreg;
            dprintln("Reg2 / Reg1 (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                   static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));
          */
          doublePointer<int32_t> dp = getRegPair2<int32_t>();

          *(dp.destreg) /= *(dp.srcreg);
          break;
        }
      default: {
          dprintln(F("INVALID INSTRUCTION:"));
        }
    }
  }
  else {

    /*  Above FIXED_WIDTH_BASE instructions. These instructions do not
        exist in more than one form (i.e. the data they operate on, if any,
        is of a single fixed width so they have a numeric value above that of
        FIXED_WIDTH_BASE, at least at present.
    */
    //  uint8_t * srcptr;
    // uint8_t * destptr;
    switch (opcode) {

      case Opcode::ADD_FL: {
          // Add srcreg to destreg and leave the result in destreg
          dprint(F("ADD_FL:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          // uint8_t targetRegisters = readData <uint8_t> ();
          /*
                    RegPair tr = getRegPair();
                    srcptr = getPtr(tr.reg1, Location::REG);
                    destptr = getPtr(tr.reg2, Location::REG);
                    float * srcreg = reinterpret_cast<float*>(srcptr);
                    float * destreg = reinterpret_cast<float*>(destptr);
                     destreg += *srcreg;
                    dprintln("Sum of register pair (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                             static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));
          */
          doublePointer<float> dp = getRegPair2<float>();

          *(dp.destreg) += *(dp.srcreg);
          break;
        }
      case Opcode::MUL_FL: {
          // Multiply srcreg and destreg and leave the result in destreg
          dprint(F("MUL_FL:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          // uint8_t targetRegisters = readData <uint8_t> ();
          /*
            RegPair tr = getRegPair();
            srcptr = getPtr(tr.reg1, Location::REG);
            destptr = getPtr(tr.reg2, Location::REG);
            float * srcreg = reinterpret_cast<float*>(srcptr);
            float * destreg = reinterpret_cast<float*>(destptr);
            destreg *= *srcreg;
            dprintln("Product of register pair (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                   static_cast<uint8_t>(PrintCategory::MATH));
          */
          doublePointer<float> dp = getRegPair2<float>();

          *(dp.destreg) *= *(dp.srcreg);
          break;
        }
      case Opcode::SUB_FL: {
          // Substract srcreg from destreg and leave the result in destreg
          dprint(F("SUB_FL:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          /*
                    //  uint8_t targetRegisters = readData <uint8_t> ();
                    RegPair tr = getRegPair();
                    srcptr = getPtr(tr.reg1, Location::REG);
                    destptr = getPtr(tr.reg2, Location::REG);
                    float * srcreg = reinterpret_cast<float*>(srcptr);
                    float * destreg = reinterpret_cast<float*>(destptr);
                     destreg -= *srcreg;
                    dprintln("Reg2 - Reg1 (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                             static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));
          */
          doublePointer<float> dp = getRegPair2<float>();

          *(dp.destreg) -= *(dp.srcreg);
          break;
        }
      case Opcode::DIV_FL: {
          /*
             Divide destreg by srcreg and leave the result in destreg. This
             allows for repeated division without reloading the srcreg.
             Example: Load srcreg with 2 and destreg with 16, divide once and you
             have srcreg = 2, destreg = 8. You can now divide again by 2 without changing
             the 2. If the operation worked the other way you would load srcreg with 16,
             and destreg with 2. Dividing now leaves you with srcreg = 16, destreg = 8.
             The system is thus not set up to divide again by the same value but its cofactor.
          */
          dprint(F("DIV_FL:"), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(OpcodeWithWidth2String(opPair), static_cast<uint8_t>(PrintCategory::STATUS));
          // uint8_t targetRegisters = readData <uint8_t> ();
          /*
                    RegPair tr = getRegPair();
                    srcptr = getPtr(tr.reg1, Location::REG);
                    destptr = getPtr(tr.reg2, Location::REG);
                    float * srcreg = reinterpret_cast<float*>(srcptr);
                    float * destreg = reinterpret_cast<float*>(destptr);
                     destreg /= *srcreg;
                    dprintln("Reg2 / Reg1 (" + String(tr.reg1) + "," + String(tr.reg2) + ") = " + String(*destreg),
                             static_cast<uint8_t>(PrintCategory::REG) & static_cast<uint8_t>(PrintCategory::MATH));
          */
          doublePointer<float> dp = getRegPair2<float>();

          *(dp.destreg) /= *(dp.srcreg);
          break;
        }
      case Opcode::SP_ADJ: {
          dprint(F("SP_ADJ: SP += "), static_cast<uint8_t>(PrintCategory::STATUS));
          uint8_t adjustment = readData <uint8_t> ();
          _SP += adjustment;
          dprintln(String(adjustment));
          break;
        }
      case Opcode::PRINT_AS: {
          dprint(F("PRINT_AS: "), static_cast<uint8_t>(PrintCategory::STATUS));
          // uint8_t nibbles = readData <uint8_t> ();
          RegPair tr = getRegPair();
          String s = getAsString(static_cast<uint8_t*>(&_reg[tr.reg2 * 4]), static_cast<DataMode>(tr.reg1));
          dprint(F("Reg "), static_cast<uint8_t>(PrintCategory::STATUS));
          dprint(String(tr.reg2), static_cast<uint8_t>(PrintCategory::STATUS));
          dprint(F(", as "), static_cast<uint8_t>(PrintCategory::STATUS));
          dprint(VM::_dataModeStrings[tr.reg1], static_cast<uint8_t>(PrintCategory::STATUS));
          dprint(F(" is "), static_cast<uint8_t>(PrintCategory::STATUS));


          dprintln(s, static_cast<uint8_t>(PrintCategory::PRINT) | static_cast<uint8_t>(PrintCategory::STATUS));

          break;
        }
      case Opcode::UJMP: {
          //dprintln(F("UJMP"), static_cast<uint8_t>(PrintCategory::STATUS));
          uint16_t addr = readData <uint16_t> ();
          _ip16 = addr;
          dprint(F("UJMP to addr: "), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(String(addr), static_cast<uint8_t>(PrintCategory::STATUS));
          break;
        }
      case Opcode::JEQ: {
          dprintln(F("JEQ"), static_cast<uint8_t>(PrintCategory::STATUS));
          uint16_t addr = readData <uint16_t> ();
          if (_cmpReg == Comparison::EQUAL ) {
            _ip16 = addr;
          } else {
            dprint(F("NO "), static_cast<uint8_t>(PrintCategory::STATUS));
          }
          dprint(F("Jump to "), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(String(addr), static_cast<uint8_t>(PrintCategory::STATUS));
          break;
        }
      case Opcode::CALL: {
          uint16_t addr = readData <uint16_t> ();
          uint8_t * ipValPtr = reinterpret_cast<uint8_t*>(&_ip16);
          _SP -= 2; // make room on stack for return address
          uint8_t * destptr = &_mem[_SP];
          dprint(F("CALL: "), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(String(addr), static_cast<uint8_t>(PrintCategory::STATUS));
          moveData(ipValPtr, destptr, 2);
          _ip16 = addr;
          break;
        }
      case Opcode::RET: {
          /*
             SUPER IMPORTANT!!!!
             ==============================================================
             Return bounces you back to the address stored in reg0. Reg0 is special
             Don't put garbage there!!!!
             ==============================================================
          */
          uint16_t * retAddr = reinterpret_cast<uint16_t*>(&_reg[0]);
          dprint(F("RET to addr in Reg0: "), static_cast<uint8_t>(PrintCategory::STATUS));
          dprintln(String(*retAddr), static_cast<uint8_t>(PrintCategory::STATUS));
          _ip16 = *retAddr;
          break;
        }
      case Opcode::NOOP:
        dprintln (F("NOOP"), static_cast<uint8_t>(PrintCategory::STATUS));
        break;
      default:
        break;
    }
  }
  //dprintln(F("After inst (sp,ip) : (" + String(_SP) + "," + String(_ip16) + ")\n"));
}

void VM::step() {
  dprintln(repeatString(F("-="), 40), static_cast<uint8_t>(PrintCategory::STATUS));
  updateBoundData();
  //dprintln(repeatString(F("B"), 5), static_cast<uint8_t>(PrintCategory::STATUS));
  printStatus();
  //dprintln(repeatString(F("C"), 5), static_cast<uint8_t>(PrintCategory::STATUS));
  //dprintln(F("*** sp: " + String(_SP) + ",ip: ") + String(_ip16), static_cast<uint8_t>(PrintCategory::REPL));
  Opcode opcode = readData <Opcode> (_ip16);
  //dprintln(F("Opcode:") + String(static_cast<uint8_t>(opcode)));
  exec(opcode);

}

void VM::updateBoundData() {
  for (uint8_t i = 0; i < NUM_PINS; i++) {
    //dprint(F("i:") + String(i));
    _pinBindings[i].updatePin(*this);

  }
}

void VM::printBindings() {
  for (uint8_t i = 0; i < NUM_PINS; i++)
    _pinBindings[i].print();
}

void VM::printMem(uint16_t startAddr, uint16_t endAddr, boolean printAsCArray) {
  dprintln(repeatString(F("M"), 20), static_cast<uint8_t>(PrintCategory::REPL));

  String divider = F("\n");
  String preamble = F("");
  String postamble = F("");
  if (printAsCArray) {
    preamble = F("[");
    postamble = F("]\n");
    divider = F(", ");
  }
  dprint(preamble, static_cast<uint8_t>(PrintCategory::REPL));
  for (uint16_t i = startAddr; i < endAddr; i++) {
    if (printAsCArray) {
      if ((i - startAddr + 1) % 10 == 0) {
        dprintln(F(""));
      }

    }
    else {
      dprint(String(i) + ": ", static_cast<uint8_t>(PrintCategory::REPL));
    }
    dprint(String(static_cast<uint8_t>(_mem[i])) + divider, static_cast<uint8_t>(PrintCategory::REPL));
  }

  dprint(postamble, static_cast<uint8_t>(PrintCategory::REPL));
}

void VM::printStack() {
  dprintln(repeatString(F("S"), 20), static_cast<uint8_t>(PrintCategory::REPL));
  for (uint16_t i = STACK_TOP - 1; i > STACK_TOP - _stackSize; i--) {
    dprintln(String(i) + ": " + String(static_cast<uint8_t>(_mem[i])), static_cast<uint8_t>(PrintCategory::REPL));
  }
}

void VM::printRegisters() {
  dprintln(repeatString(F("R"), 20), static_cast<uint8_t>(PrintCategory::REPL));
  for (uint8_t i = 0; i < 16 ; i++) {
    uint32_t * regptr32 = reinterpret_cast<uint32_t*>(&_reg[i * 4]);
    dprint(F("Reg "), static_cast<uint8_t>(PrintCategory::REPL));
    dprint(String(i), static_cast<uint8_t>(PrintCategory::REPL));
    dprint(F(": "), static_cast<uint8_t>(PrintCategory::REPL));
    dprintln(String(*regptr32), static_cast<uint8_t>(PrintCategory::REPL));
  }
}

void VM::reset() {
  _ip16 = 0;
  _SP = 0;
}

