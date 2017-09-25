#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"
#include "Cell.h"


Cell::Cell() {}
Cell::~Cell() {};
boolean Cell::exec(VM & vm) {
  boolean rval = true;
  dprintln("BEGIN Cell::exec(, [");
  if (_ct < add2) {
    // add2 is the first non-data tyoe. Data type automatically push
    // onto the stack

    dprint("Cell is data of type: ");
    switch (_ct) {
      case CT_i:
        dprintln("Int32");
        break;
      case CT_f:
        dprintln("Float");
        break;
      case CT_str:
        dprintln("String");

        break;
    }
    vm.push(this);
  }
  else {
    switch (_ct) {
      case delay_t: {
          dprint("Delay (");
          Cell * msec =  vm.pop();
          dprintln(String(msec->_d.ival) + " ms)");
          delay(msec->_d.ival);
          break;
        }
      case bindAI: {
          Cell * pinCell;
          Cell * memCell;
          vm.loadBinding(pinCell, memCell);


          vm.setPinIO(pinCell->_d.ival, INPUT);
          vm.setPinAD(pinCell->_d.ival, true);
          vm.setPinAddress(pinCell->_d.ival, memCell->_d.ival);
          dprintln("End bindAI--");
          break;
        }
        case bindAO: {
          Cell * pinCell;
          Cell * memCell;
          vm.loadBinding(pinCell, memCell);


          vm.setPinIO(pinCell->_d.ival, OUTPUT);
          vm.setPinAD(pinCell->_d.ival, true);
          vm.setPinAddress(pinCell->_d.ival, memCell->_d.ival);
          dprintln("End bindAO--");
          break;
        }
    }
  }

  dprintln("END Cell::exec(, ]");
  return rval;
}
Cell::Cell(CellType ct, Data d) {
  _ct = ct;
  _d = d;
}

Cell::Cell(CellType ct) {
  _ct = ct;
  _d.ival = 0; // For types that contain no actual data we zero the data field anyway.
}

void Cell::print() {
  switch (_ct) {
    case CT_i:
    break;
    case CT_f:
    break;
    case CT_str:
    break;
  }
}

