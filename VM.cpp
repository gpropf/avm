#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"
#include "Cell.h"

#include "Number.h"
VM::PinBinding::PinBinding() {
  _mode = 255; // They all begin life de-activated.
}

void VM::PinBinding::setIO(uint8_t m) {
  _mode = m;
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
  if (_mode == 255) {
    dprintln("Pin " + String(_pin) + " inactive");
    return;
  }
    
  if (_ad) {
    switch (_mode) {
      case OUTPUT:
        dprint("AnalogWrite to pin #: " + String(_pin) + ", ");


        //dprint(", val = " + String(valCell->toInt()) + ", ");
        analogWrite(_pin, vm.readMem(_address));
        //vm.push(valCell);

        //dprintln("leaving AnalogWrite::exec()--");
        break;

    }
  }
  else {

  }
}

Cell * VM::readCell(uint16_t i) {
  return _mem[i];
}



uint8_t  VM::readMem(uint16_t i) {
  return _progmem[i];
}

void VM::loadBinding(Cell * pinCell, Cell * memCell) {
  dprint("void loadBinding(), ");
  pinCell = pop();
  dprint("PIN#:" + String(pinCell->_d.ival));
  memCell = pop();
  dprint(", mem location = " + String(memCell->_d.ival) + ", ");
}

void VM::setPinIO(uint8_t pin, uint8_t m) {
  pinMode(pin, m);
  _bindings[pin].setIO(m);
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

void VM::appendCell(Cell * c) {
  dprintln("VM::appendCell() -- _AP:" + String(_AP));
  _mem[_AP] = c;
  _AP++;


}

VM::VM(uint16_t memSize, uint16_t stackSize):  _memSize(memSize), _stackSize(stackSize) {

  _mem = new Cell*[memSize];
  _stack = new Cell*[stackSize];
  _progmem = new uint8_t[memSize];
  _AP = 0;
  //_IP = 0;
  //_SP = 0;
  //Number n = Number();
}

void VM::writeCell(Cell * c, uint16_t i) {
  _mem[i] = c;
}

void VM::step() {
  updateBoundData();
  dprint("IP:" + String(_IP) + ", " + "SP:" + String(_SP) + ":");
  dprint(" / ");
  boolean incIP = _mem[_IP]->exec(*this);
  if (incIP == true)
    _IP++;
}

void VM::updateBoundData() {
  for (uint8_t i = 0; i < NUM_PINS; i++) {
    _bindings[i].updatePin(*this);
  }
}


Cell * VM::pop() {
  if (_SP < 1 )
    return nullptr;
  dprint("pop - SP:" + String(_SP) + ", ");
  return _stack[--_SP];
}

void VM::push(Cell *c) {
  dprintln("BEGIN vm.push");
  if (_SP >= _stackSize ) {
    dprintln("Stack Overflow!!!!!");
    return;
  }
  dprint("push:");// + String(static_cast<Int32*>(c)->toInt()) + ", SP:" + String(_SP) + ", ");
  _stack[_SP++] = c;
  //dprintln("push 2 - SP:" + String(_SP));
  //_IP++;
}

void VM::printStack() {
  for (uint16_t i = 0; i < _stackSize; i++) {
    _stack[i]->print();
  }
}

void VM::reset() {
  _IP = 0;
  _SP = 0;
}

