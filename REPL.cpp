#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"
#include "REPL.h"



void VM::memdump(uint16_t startaddr, uint16_t endaddr) {
  
}

String REPL::readCommand() {
  char readBuf[READ_BUFFER_LENGTH];
  uint8_t bytesRead = 0;
  String cmd = "";

  while (true) {
    for (uint8_t i = 0; i < READ_BUFFER_LENGTH; i++) {
      readBuf[i] = 0;
    }
    if (Serial.available() > 0) {
      bytesRead = Serial.readBytes(readBuf, READ_BUFFER_LENGTH - 1);

      if (bytesRead > 0) {
        //dprintln(String(bytesRead) + ":" + String(readBuf));

        char c = readBuf[bytesRead - 1];
        //dprintln(String("c:" + String((uint8_t)c)));
        if (c == '\n') {
          readBuf[bytesRead - 1] = 0;
          //readBuf[bytesRead] = 0;
          // dispatch command
          cmd += String(readBuf);
          dprintln("(" + cmd + ")");
          //cmd = "";
          Serial.flush();
          return cmd;
        }
        else {
          readBuf[bytesRead] = 0;
          cmd += String(readBuf);
        }

        for (uint8_t i = 0; i < READ_BUFFER_LENGTH; i++) {
          readBuf[i] = 0;
        }
      }
    }

    bytesRead = 0;
    delay(REPL_DELAY);
  }

}

String REPL::getPromptString(String subPrompt) {
  String tabs;
  if (subPrompt != "") {
    tabs = "\t";
  }
  return tabs + _leftPrompt + String(eptr) + _rightPrompt + subPrompt;
}

void REPL::loop(String subPrompt) {

  while (true) {
    dprint(getPromptString(subPrompt));
    switch (_runMode) {
      case STEP: {

          String cmd = readCommand();
          dprintln(cmd);
          evalCommand(cmd);
          break;
        }
      case RUN: {
          dprintln("");
          _vm->step();
          break;
        }
      default:
        break;
    }
  }
}

void REPL::evalCommand(String cmd) {
  char firstChar = cmd.charAt(0);

  switch (firstChar) {
    case 'p':
      _vm->memdump();
      break;
    case 'r':
      _runMode = RUN;
      break;
    case 's':
      //if (_steppingMode)
      _vm->step();
      break;
    default:
      break;
  }


  uint16_t cellNum = cmd.toInt();
  /*
  if (cellNum != 0) {
    Atom * a = _lm->_mm->getptr(cellNum);
    a->print(20);
  }
  */
}

VM * REPL::bindVM(VM * vm) {
  VM *oldVM = _vm;
  _vm = vm;
  return oldVM;
}

void REPL::toggleStepping() {
  _steppingMode = !_steppingMode;
}

