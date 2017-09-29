#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"
#include "REPL.h"





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
          parseCommand(cmd);
         // evalCommand(cmd);
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


void REPL::parseCommand(String s)
{
  int startIndex = 0;
  //String action;
  String args[5];
  uint8_t i = 0;
  dprintln("parsing..." + s);
  int spaceAt = 0;
  dprintln("s init:" + s);
  while (s != "" && i < 4 && spaceAt != -1) {
    spaceAt = s.indexOf(" ");
    if (spaceAt == -1) {
      args[i] = s;



    }
    else {
      args[i] = s.substring(startIndex, spaceAt);
      s = s.substring(spaceAt + 1);
    }
    dprintln("s:" + s);
    i++;
    // b ao 4 556

  }

  for (uint8_t j = 0; j < i; j++) {
    dprintln("Arg " + String(j) + ": " + args[j]);
  }
  String &action = args[0];
  if (action == "b" || action == "B") {
    
    
    if (i < 3) {
      dprintln(F("ERROR: Binding syntax: B pin address <AO|AI|DO|DI>"));
      return;
    }
    uint8_t pin = args[1].toInt();
    boolean ad; // FIXME
    char c = args[3].charAt(0);
    switch (c) {
      case 'a':
        ad = true;
        break;
      case 'd':
        ad = false;
        break;
      default:
        dprintln(F("ERROR: pin io must be analog ('a') or digital ('d')"));
    }
    uint8_t io;
    c = args[3].charAt(1);
    switch (c) {
      case 'o':
        io = OUTPUT;
        break;
      case 'i':
        io = INPUT;
        break;
      case 'p':
        io = INPUT_PULLUP;
        break;
      default:
        dprintln(F("ERROR: pin io must be input ('a'), input pullup ('p'), or output ('o')"));
    }
    

    uint16_t addr = args[2].toInt();
    _vm->createBinding(pin, io, ad, addr);
    //_vm->set
  }
  else if (action == "p" || action == "P") {
    _vm->printBindings();
  }
   else if (action == "m" || action == "M") {
    _vm->printMem(args[1].toInt(),args[2].toInt());
  }
  else if (action == "s" || action == "S") {
    _vm->step();
  }
}



VM * REPL::bindVM(VM * vm) {
  VM *oldVM = _vm;
  _vm = vm;
  return oldVM;
}

void REPL::toggleStepping() {
  _steppingMode = !_steppingMode;
}

