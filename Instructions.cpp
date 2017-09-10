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

boolean AnalogWrite::exec (VM & vm) {
  Serial.print("boolean AnalogWrite::exec(), ");
  Int32 * pinCell =  static_cast<Int32 *>(vm.pop());
  Serial.print("PIN#:" + String(pinCell->toInt()));
  Int32 * valCell = static_cast<Int32 *>(vm.pop());
  Serial.print(", val = " + String(valCell->toInt()) + ", ");
  analogWrite(pinCell->toInt(), valCell->toInt());
  vm.push(valCell);

  Serial.println("leaving AnalogWrite::exec()--");
  return true;
}

boolean Halt::exec (VM & vm) {
  Serial.print("HALTING!!!! Halt::exec(), ");
  
  Serial.println("leaving Halt::exec()--");
  return false;
}

boolean AnalogRead::exec (VM & vm) {
  Serial.print("AnalogRead::exec(), ");
  Int32 * pinCell =  static_cast<Int32 *>(vm.pop());
  Serial.print("PIN#:" + String(pinCell->toInt()));
  Int32 * valCell = new Int32(vm.readPin(static_cast<uint8_t>(pinCell->toInt()), true));
  Serial.print(", val = " + String(valCell->toInt()) + ", ");
  vm.push(valCell);

  Serial.println("leaving AnalogRead::exec()--");
  return true;
}

boolean DigitalRead::exec (VM & vm) {
  //static_cast<uint8_t> val =

  //vm.readPin(static_cast<Int32 *>(vm.pop()),false);
  return true;
}

boolean Delay::exec(VM & vm) {
  Serial.print("Delay::exec() [");
  
  Int32 * msec =  static_cast<Int32 *>(vm.pop());
  Serial.println("]");
  delay(msec->toInt());
  Serial.print("Ending Delay::exec() [");
  return true;
};

boolean Add2::exec(VM & vm) {
  Serial.print("Add2::exec() [");
  // Number * n = new Number();
  Number * a1 = static_cast<Number *>(vm.pop());
  Number * a2 = static_cast<Number *>(vm.pop());
  int sum = a1->toInt() + a2->toInt();
  Int32 * isum32 = new Int32(sum);
  vm.push(isum32);
  Serial.println("] Sum:" + String(sum));
  return true;
};

boolean JmpIfGreater::exec(VM & vm) {
  Serial.print("JmpIfGreater::exec(), ");
  //Serial.println("Number::exec()");
  // Number * n = new Number();
  int v1 = static_cast<Int32 *>(vm.pop())->toInt();
  int  v2 = static_cast<Int32 *>(vm.pop())->toInt();
  uint16_t addr = static_cast<Int32 *>(vm.pop())->toInt();
  Serial.println(", v1:" + String(v1) + " ,v2:" + String(v2) + " ,addr:" + String(addr));
  if (v1 < v2) {
    vm.setIP(addr);
    Serial.println("Resetting IP=" + String(addr));
    vm.setSP(0); // FIXME:FIXME:FIXME!!!!! This is a hack to see what the stack problem was.
    return false;
  }
  else {
    Serial.println("IP Unchanged (Would have been set to " + String(addr) + ")");
    return true;
  }

};
