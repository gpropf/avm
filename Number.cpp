#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"
#include "Cell.h"
#include "Number.h"

Number::Number() {}
Number::~Number() {}


boolean Number::exec(VM & vm) {

  //
  // Number * n = new Number();

  vm.push(this);
  Serial.println("Number: " + String(this->toInt()));
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

int Int32::toInt() {
  return _ival;
}

