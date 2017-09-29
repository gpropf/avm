#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"

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

}

void VM::PinBinding::setAD(boolean ad) {
  _ad = ad;
}
boolean VM::PinBinding::getAD() {

}


void VM::PinBinding::setPin(uint8_t pin) {
  _pin = pin;
}

void VM::PinBinding::setAddress(uint16_t address) {
  _address = address;
}

uint8_t VM::PinBinding::getPin() {

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

          uint8_t val = vm.readMem(_address);
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




uint8_t  VM::readMem(uint16_t i) {
  return _mem[i];
}



void VM::setPinIO(uint8_t pin, uint8_t m) {

  _pinBindings[pin].setIO(m);
}
void VM::setPinAD(uint8_t pin, boolean ad) {

}
void VM::setPinAddress(uint8_t pin, uint16_t addr) {

}

uint8_t VM::readPin(uint8_t pin, boolean isAnalog) {

  int readVal = 0;
  if (isAnalog)
    readVal = analogRead(pin);
  else
    readVal = digitalRead(pin);
  return readVal;
}


VM::VM(uint16_t memSize, uint16_t stackSize):  _memSize(memSize), _stackSize(stackSize) {

  _mem = new uint8_t[memSize];
  _stack = new uint8_t[stackSize];
  _progmem = new uint8_t[memSize];
  _AP = 0;
  for (uint16_t i = 0; i < memSize; i++)
    _mem[i] = i;
  //_IP = 0;
  //_SP = 0;
  //Number n = Number();
}


void VM::step() {
  updateBoundData();
  dprint("IP:" + String(_IP) + ", " + "SP:" + String(_SP) + ":");
  dprint(" / ");
  //boolean incIP = _mem[_IP]->exec(*this);
  boolean incIP = false;
  if (incIP == true)
    _IP++;
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
  _IP = 0;
  _SP = 0;
}


