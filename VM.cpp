#include "Arduino.h"

#include "VM.h"
#include "Cell.h"

#include "Number.h"


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
  Serial.println("IP:" + String(_IP) + ", " + "SP:" + String(_SP));
  _mem[_IP]->exec(*this);
  _IP++;
}

Cell * VM::pop() {
  if (_SP < 1 )
    return nullptr;
  //Serial.println("pop - SP:" + String(_SP));
  return _stack[--_SP];
}

void VM::push(Cell *c) {
  if (_SP >= _stackSize )
    return;
  Serial.println("push:" + String(static_cast<Int32*>(c)->toInt()) + ", SP:" + String(_SP));
  _stack[_SP++] = c;
  //Serial.println("push 2 - SP:" + String(_SP));
  //_IP++;
}

