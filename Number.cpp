#include "Arduino.h"

#include "VM.h"
#include "Cell.h"
#include "Number.h"

Number::Number() {}
Number::~Number() {}


void Number::exec(VM & vm) {
 
  //Serial.println("Number::exec()");
  // Number * n = new Number();
 
  vm.push(this);
  //vm._IP++;
  //vm.incIP();
 // vm.incIP();
  //this->Cell::exec(vm);
};

Int32::Int32(int i) {
  _ival = i;
}

Int32::Int32() {}

Int32::~Int32() {}

int  Int32::toInt() {
  return _ival;
}

