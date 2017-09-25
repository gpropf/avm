#include "Arduino.h"
#include "config.h"
#include "util.h"



#include "VM.h"
#include "Cell.h"
#include "Instructions.h"
#include "Number.h"
/*
  void Pop::exec(VM & vm) {
  //dprintln("Number::exec()");
  // Number * n = new Number();
  Cell * c = vm.pop();
  dprintln("Pop (throwaway)");
  };

  void Push::exec(VM & vm) {
  //dprintln("Number::exec()");
  // Number * n = new Number();
  Cell * c = vm.pop();
  dprintln("Pop (throwaway)");
  };
*/

boolean PinMode::exec (VM & vm) {
  dprint("boolean PinMode::exec(), [");
  Int32 * pinCell =  static_cast<Int32 *>(vm.pop());
  int pin = pinCell->toInt();
  dprint("PIN#:" + String(pin) + ", ");
  Int32 * valCell = static_cast<Int32 *>(vm.pop());
  int val = valCell->toInt();
  dprint(", val2 = " + String(val) + ", ");
 // pinMode(pin, val);
  //vm.push(valCell);

  dprintln("leaving PinMode::exec()--");
  return true;
}




boolean BindAnalogIn::exec (VM & vm) {
   dprint("boolean BindAnalogIn::exec(), [");
  Cell * pinCell;
  Cell * memCell;
  vm.loadBinding(pinCell, memCell);
  
  
  vm.setPinIO(pinCell->_d.ival, INPUT);
  vm.setPinAD(pinCell->_d.ival, true);
  vm.setPinAddress(pinCell->_d.ival, memCell->_d.ival);
  dprintln("leaving PinMode::exec()--");
  return true;
}

boolean AnalogWrite::exec (VM & vm) {
  dprint("boolean AnalogWrite::exec(), [");
  Int32 * pinCell =  static_cast<Int32 *>(vm.pop());
  dprint("PIN#:" + String(pinCell->toInt()));
  Int32 * valCell = static_cast<Int32 *>(vm.pop());
  dprint(", val = " + String(valCell->toInt()) + ", ");
  analogWrite(pinCell->toInt(), valCell->toInt());
  //vm.push(valCell);

  dprintln("leaving AnalogWrite::exec()--");
  return true;
}

boolean Halt::exec (VM & vm) {
  dprint("HALTING!!!! Halt::exec(), [");

  dprintln("leaving Halt::exec()--");
  return false;
}

boolean AnalogRead::exec (VM & vm) {
  dprint("AnalogRead::exec(), [");
  Int32 * pinCell =  static_cast<Int32 *>(vm.pop());
  dprint("PIN#:" + String(pinCell->toInt()));
  Int32 * valCell = new Int32(vm.readPin(static_cast<uint8_t>(pinCell->toInt()), true));
  dprint(", val = " + String(valCell->toInt()) + ", ");
  vm.push(valCell);

  dprintln("leaving AnalogRead::exec()--");
  return true;
}

boolean DigitalRead::exec (VM & vm) {
  dprint("DigitalRead::exec(), [");
  Int32 * pinCell =  static_cast<Int32 *>(vm.pop());
  dprint("PIN#:" + String(pinCell->toInt()));
  Int32 * valCell = new Int32(vm.readPin(static_cast<uint8_t>(pinCell->toInt()), false));
  dprint(", val = " + String(valCell->toInt()) + ", ");
  vm.push(valCell);

  dprintln("leaving DigitalRead::exec()--");
  return true;
}

boolean Delay::exec(VM & vm) {
  dprint("Delay::exec() [");

  Int32 * msec =  static_cast<Int32 *>(vm.pop());
  dprintln("]");
  delay(msec->toInt());
  dprint("Ending Delay::exec() [");
  return true;
};

boolean Add2::exec(VM & vm) {
  dprint("Add2::exec() [");
  // Number * n = new Number();
  Number * a1 = static_cast<Number *>(vm.pop());
  Number * a2 = static_cast<Number *>(vm.pop());
  int sum = a1->toInt() + a2->toInt();
  Int32 * isum32 = new Int32(sum);
  vm.push(isum32);
  dprintln("] Sum:" + String(sum));
  return true;
};

boolean JmpIfGreater::exec(VM & vm) {
  dprint("JmpIfGreater::exec(),[");
  //dprintln("Number::exec()");
  // Number * n = new Number();
  int v1 = static_cast<Int32 *>(vm.pop())->toInt();
  int  v2 = static_cast<Int32 *>(vm.pop())->toInt();
  uint16_t addr = static_cast<Int32 *>(vm.pop())->toInt();
  dprintln(", v1:" + String(v1) + " ,v2:" + String(v2) + " ,addr:" + String(addr));
  if (v1 < v2) {
    vm.setIP(addr);
    dprintln("Resetting IP=" + String(addr));
    vm.setSP(0); // FIXME:FIXME:FIXME!!!!! This is a hack to see what the stack problem was.
    return false;
  }
  else {
    dprintln("IP Unchanged (Would have been set to " + String(addr) + ")");
    return true;
  }

};
