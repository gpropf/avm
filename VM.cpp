#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"
#include "Cell.h"

#include "Number.h"

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

void VM::loadBinding(Int32 * pinCell, Int32 * memCell) {
  Serial.print("void loadBinding(), ");
  pinCell =  static_cast<Int32 *>(pop());
  Serial.print("PIN#:" + String(pinCell->toInt()));
  memCell = static_cast<Int32 *>(pop());
  Serial.print(", mem location = " + String(memCell->toInt()) + ", ");
}

void VM::setPinIO(uint8_t pin, uint8_t m) {
  pinMode(pin, m);
  bindings[pin].setIO(m);
}
void setPinAD(uint8_t pin, boolean ad) {

}
void setPinAddress(uint8_t pin, uint16_t addr) {

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
  Serial.println("VM::appendCell() -- _AP:" + String(_AP));
  _mem[_AP] = c;
  _AP++;


}

VM::VM(uint16_t memSize, uint16_t stackSize):  _memSize(memSize), _stackSize(stackSize) {

  _mem = new Cell*[memSize];
  _stack = new Cell*[stackSize];
  _AP = 0;
  //_IP = 0;
  //_SP = 0;
  //Number n = Number();
}

void VM::writeCell(Cell * c, uint16_t i) {
  _mem[i] = c;
}

void VM::step() {
  Serial.print("IP:" + String(_IP) + ", " + "SP:" + String(_SP) + ":");
  Serial.print(" / ");
  boolean incIP = _mem[_IP]->exec(*this);
  if (incIP == true)
    _IP++;
}

Cell * VM::pop() {
  if (_SP < 1 )
    return nullptr;
  Serial.print("pop - SP:" + String(_SP) + ", ");
  return _stack[--_SP];
}

void VM::push(Cell *c) {
  if (_SP >= _stackSize )
    return;
  Serial.print("push:" + String(static_cast<Int32*>(c)->toInt()) + ", SP:" + String(_SP) + ", ");
  _stack[_SP++] = c;
  //Serial.println("push 2 - SP:" + String(_SP));
  //_IP++;
}

void VM::printStack() {

}

void VM::reset() {
  _IP = 0;
  _SP = 0;
}

