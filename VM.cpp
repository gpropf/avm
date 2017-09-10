#include "Arduino.h"

#include "VM.h"
#include "Cell.h"

#include "Number.h"

uint8_t VM::readPin(uint8_t pin, boolean isAnalog) {

  int readVal = 0;
  if (isAnalog)
    readVal = analogRead(pin);
  else
    readVal = digitalRead(pin);
  return readVal;
}


VM::VM(uint16_t memSize, uint16_t stackSize):  _memSize(memSize), _stackSize(stackSize) {

  _mem = new Cell*[memSize];
  _stack = new Cell*[stackSize];
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

