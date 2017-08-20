#include "Arduino.h"



#include "VM.h"
#include "Cell.h"
#include "Instructions.h"
#include "Number.h"
/*
void Pop::exec(VM & vm) {
  //Serial.println("Number::exec()");
  // Number * n = new Number();
  Cell * c = vm.pop();
  Serial.println("Pop (throwaway)");
};

void Push::exec(VM & vm) {
  //Serial.println("Number::exec()");
  // Number * n = new Number();
  Cell * c = vm.pop();
  Serial.println("Pop (throwaway)");
};
*/
void Add2::exec(VM & vm) {
  //Serial.println("Number::exec()");
  // Number * n = new Number();
  Number * a1 = static_cast<Number *>(vm.pop());
  Number * a2 = static_cast<Number *>(vm.pop());
  int sum = a1->toInt() + a2->toInt();
  Int32 * isum32 = new Int32(sum);
  vm.push(isum32);
  Serial.println("Sum:" + String(sum));
};
