#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"


/*
void VM::writeInt32(int32_t i32, boolean advanceMemAddr) {
  int32_t * i32p = static_cast<int32_t*>(_ip16);
  *i32p = i32;
  if (advanceMemAddr) {
    _ip16 += sizeof(int32_t);
  }
}

void VM::writeByte(uint8_t i32, boolean advanceMemAddr) {
   uint8_t * ui8p = static_cast<uint8_t*>(_ip16);
  *ui8p = i32;
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
  }
  dprintln("Pin " + String(_pin) + ", addr:" + String(_address) + ", mode:" + modeStr);
}

VM::PinBinding::PinBinding() {
  _io = 255; // They all begin life de-activated.
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
  if (_io == 255) {
    //dprintln("Pin " + String(_pin) + " inactive");
    return;
  }

  if (_ad) {
    switch (_io) {
      case OUTPUT: {
          //dprint("AnalogWrite to pin #: " + String(_pin) + ", ");

          uint8_t val = vm.readData <uint8_t> (_address);
          dprint("Writing " + String(val) + " to pin " + String(_pin));
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

  }
  dprintln("");
}

/*
 uint8_t  VM::readMem(uint16_t i) {
  return _mem[i];
}
*/

VM::VM(uint16_t memSize, uint16_t stackSize):  _memSize(memSize), _stackSize(stackSize) {

 _mem = new uint8_t[memSize];
 _ip16 = 0;
  _stack = new uint8_t[stackSize];
  //_progmem = new uint8_t[memSize];
  //_ip16 = new uint8_t[memSize];
  _ip16Copy = _ip16;
  _AP = 0;
  for (uint16_t i = 0; i < memSize; i++)
    _mem[i] = i;
}

void VM::exec(Opcode opcode) {
  switch(opcode) {
    case Opcode::BINDAI: {
      uint8_t pin = readData <uint8_t> ();
      uint16_t addr = readData <uint16_t> ();
      createBinding(pin, INPUT, true, addr);
    }
  }
}


void VM::step() {
  updateBoundData();
  dprint("IP:" + String(static_cast<uint16_t>(_ip16)) + ", " + "SP:" + String(_SP) + ":");
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
  for (uint8_t i = startAddr; i < endAddr; i++)
    dprintln(String(i) + ": " + String(static_cast<uint8_t>(_mem[i])));
}

void VM::printStack() {
  for (uint16_t i = 0; i < _stackSize; i++) {
    //   _stack[i]->print();
  }
}

void VM::reset() {
  _ip16 = 0;
  _SP = 0;
}

