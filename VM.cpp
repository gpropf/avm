#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"


/*
  void VM::writeInt32(int32_t i32, boolean advanceMemAddr) {
  int32_t * i32p = static_cast<int32_t*>(_ip16);
   i32p = i32;
  if (advanceMemAddr) {
    _ip16 += sizeof(int32_t);
  }
  }

  void VM::writeByte(uint8_t i32, boolean advanceMemAddr) {
   uint8_t * ui8p = static_cast<uint8_t*>(_ip16);
   ui8p = i32;
  if (advanceMemAddr) {
    _ip16 += sizeof(uint8_t);
  }
  }
*/

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

String VM::DM2String(DataMode dm) {
  DataMode generalType = static_cast<DataMode>(static_cast<uint8_t>(dm) & static_cast<uint8_t>(DataMode::GENERAL_TYPE_MASK));
  String s = "";
  switch (generalType) {
    case DataMode::UINT:
      s += "UINT";
      break;
    case DataMode::INT:
      s += "INT";
      break;
    case DataMode::FLOAT:
      s += "FLOAT";
      break;
    case DataMode::STRING:
      s += "STRING";
      break;
    default:
      break;

  }
  DataMode widthType = static_cast<DataMode>(static_cast<uint8_t>(dm) & static_cast<uint8_t>(DataMode::WIDTH_TYPE_MASK));
  switch (widthType) {
    case DataMode::WIDTH8:
      s += "8";
      break;
    case DataMode::WIDTH16:
      s += "16";
      break;
    case DataMode::WIDTH32:
      s += "32";
      break;
    default:
      break;
  }
  return s;
}


void VM::printStatus() {
  String amString = (_am == AddressingMode::REL)? "Relative":"Absolute";
  dprintln("IP:" + String(static_cast<uint16_t>(_ip16)) + ", "
           + "SP:" + String(_SP) + ", Data Mode: " + DM2String(_dm)
           + ", Address Mode: " + amString);
}

void VM::transferData(uint16_t addr, uint8_t * buf, DataMode dm,
                      boolean toStack, boolean adjustSP,
                      boolean alterMemory) {
  int8_t dataLength = 0;
  // uint8_t * returnBuffer;

  switch (dm) {
    case DataMode::UINT8:
    case DataMode::INT8:
      dataLength = 1;
      dprintln("transferData: data length = 1");
      break;
    case DataMode::UINT16:
    case DataMode::INT16:
      dataLength = 2;
      break;
    case DataMode::UINT32:
    case DataMode::INT32:
      dataLength = 4;
      break;
    case DataMode::FLOAT:
      dataLength = 4;
      break;
    case DataMode::STRING:
      // FIXME: find the end of the string
      break;
    default:
      //dataLength = 0;
      break;
  }

  if (toStack) {
    for (uint16_t i = 0; i < dataLength; i++) {
      if (!alterMemory) {
        _stack[_SP + i] = buf[i];
      }
      else {
        _stack[_SP + i] = _mem[addr + i];
      }
    }
  }
  else {
    for (uint16_t i = 0; i < dataLength; i++) {
      if (!alterMemory) {
        buf[i] = _stack[_SP - dataLength + i];
      }
      else {
        _mem[addr + i] = _stack[_SP - dataLength + i];
      }
    }

    dataLength = -dataLength;
  }
  if (adjustSP)
    _SP += dataLength;
}

VM::VM(uint16_t memSize, uint16_t stackSize):  _memSize(memSize), _stackSize(stackSize) {

  _dm = DataMode::UINT8;
  _am = AddressingMode::ABS;
  _mem = new uint8_t[memSize];
  _ip16 = 0;
  _SP = 0;
  _stackSize = stackSize;
  _stack = new uint8_t[stackSize];
  //_progmem = new uint8_t[memSize];
  //_ip16 = new uint8_t[memSize];
  _ip16Copy = _ip16;
  _AP = 0;

  // Fills the memory and stack with some values for now to show that it's working
  for (uint16_t i = 0; i < _stackSize; i++)
    _stack[i] = 0;
  for (uint16_t i = 0; i < memSize; i++)
    _mem[i] = i;
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

void VM::exec(Opcode opcode) {

  uint8_t * buf = NULL;
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
  else if (opcode == Opcode::PUSH || opcode == Opcode::POP) {
    uint16_t addr = readData <uint16_t> ();
    if (_am == AddressingMode::REL)
      addr = readData <uint16_t> (addr, false);
      
    if (opcode == Opcode::PUSH)
      transferData(addr, buf, _dm, true);
    else if (opcode == Opcode::POP)
      transferData(addr, buf, _dm, false);
  }
  else if (opcode == Opcode::ADD || opcode == Opcode::SUB ||
           opcode == Opcode::MUL || opcode == Opcode::DIV) {

    uint8_t * firstArg = new uint8_t[4];

    uint8_t * secondArg = new uint8_t[4];
    zeros(firstArg, 4);
    zeros(secondArg, 4);
    transferData(0, firstArg, _dm, false, true, false);
    transferData(0, secondArg, _dm, false, true, false);
    // Make 4 byte buffers for all ops
    DataMode generalType = static_cast<DataMode>(static_cast<uint8_t>(_dm) & 0b0011);
    switch (opcode) {

      case Opcode::ADD: {

          switch (generalType) {
            case DataMode::UINT: {
                uint32_t * sum = new uint32_t;
                *sum = *reinterpret_cast<uint32_t*>(firstArg) + *reinterpret_cast<uint32_t*>(secondArg);
                transferData(0, reinterpret_cast<uint8_t*>(sum), _dm, true, true, false);
                delete sum;
                break;
              }
            case DataMode::INT: {
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
      case Opcode::SUB:
      case Opcode::MUL:
      case Opcode::DIV:
        break;

      default:
        break;
    }
    delete firstArg;
    delete secondArg;

  }
  else {
    switch (opcode) {
      case Opcode::DATA_FLOAT:
        _dm = DataMode::FLOAT;
        dprintln ("DataMode::FLOAT;");
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
  for (uint16_t i = startAddr; i < endAddr; i++)
    dprintln(String(i) + ": " + String(static_cast<uint8_t>(_mem[i])));
}

void VM::printStack() {
  for (uint16_t i = 0; i < _stackSize; i++) {
    dprintln(String(i) + ": " + String(static_cast<uint8_t>(_stack[i])));
  }
}

void VM::reset() {
  _ip16 = 0;
  _SP = 0;
}

