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

        char c = readBuf[bytesRead - 1];
        if (c == '\n') {
          readBuf[bytesRead - 1] = 0;

          // dispatch command
          cmd += String(readBuf);
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
  return tabs + _leftPrompt + String(_eptr++) + _rightPrompt + subPrompt;
}

void REPL::loop(String subPrompt) {

  while (true) {
    dprint(getPromptString(subPrompt), static_cast<uint8_t>(PrintCategory::REPL));
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

uint8_t REPL::hexToDec(char hex) {
  if (hex < 65)
    return hex - 48;
  return hex - 87;
}

void REPL::loadProgram(String programStr, uint16_t addr) {

  for (uint16_t i = 0; i < programStr.length(); i += 2) {
    uint8_t lowByte = programStr.charAt(i + 1);
    uint8_t highByte = programStr.charAt(i);
    lowByte = hexToDec(lowByte);
    highByte = hexToDec(highByte);
    highByte = highByte * 16 + lowByte;
    dprint(String(lowByte) + ":" + String(highByte) + "\n", static_cast<uint8_t>(PrintCategory::REPL));
    _vm->_mem[addr++] = highByte;
  }
}

void REPL::parseCommand(String s)
{
  /* --------------------------------
      Command language is a super simple
      Lisp-esque thing. All valid commands
      have the format: Action arg1 arg2 arg3
      where Action is the action name and there
      are 3 optional arguments


  */

  /*
     B pin address <AO|AI|DO|DI>: Bind a pin to a memory address
     M start_address end_address: Print the contents of memory between the listed addresses
     P: print all pin bindings
     S: Step one instruction ahead
     Q: Print the stack
     ~: Print status
     C: Cast something in memory as one of u8, u16, u32, i8, i16, i32, fl, str
     L: Load program in args[2] into address specified in args[1]
     I: Set IP to the value in args[1]
  */

  int startIndex = 0;
  //String action;
  String args[5];
  uint8_t i = 0;
  //dprintln("parsing..." + s);
  int spaceAt = 0;
  //dprintln("s init:" + s);
  while (s != "" && i < 4 && spaceAt != -1) {
    spaceAt = s.indexOf(" ");
    if (spaceAt == -1) {
      args[i] = s;
    }
    else {
      args[i] = s.substring(startIndex, spaceAt);
      s = s.substring(spaceAt + 1);
    }
    //dprintln("s:" + s);
    i++;
  }
  /*
    for (uint8_t j = 0; j < i; j++) {
      dprintln("Arg " + String(j) + ": " + args[j]);
    }
  */
  String &action = args[0];
  if (action == "b" || action == "B") {
    // Bind command syntax is "[bB] pin address <AO|AI|DO|DI>"

    if (i < 3) {
      dprintln(F("ERROR: Binding syntax: B pin address <AO|AI|DO|DI>"));
      return;
    }
    uint8_t pin = args[1].toInt();
    boolean ad = true; // to get rid of annoying compiler warnings about possible undefined value.
    char c = args[3].charAt(0);
    // First character determines whether we have an analog or digital pin
    switch (c) {
      case 'a':
        ad = true;
        break;
      case 'd':
        ad = false;
        break;
      default:
        dprintln(F("ERROR: pin io must be analog ('a') or digital ('d')"),
                 static_cast<uint8_t>(PrintCategory::REPL));
    }
    uint8_t io = 0; // to get rid of annoying compiler warnings about possible undefined value.
    c = args[3].charAt(1);
    // Second character determines whether we have an input, output, or output pullup pin
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
        dprintln(F("ERROR: pin io must be input ('a'), input pullup ('p'), or output ('o')"),
                 static_cast<uint8_t>(PrintCategory::REPL));
    }


    uint16_t addr = args[2].toInt();
    _vm->createBinding(pin, io, ad, addr);
    //_vm->set
  }
  else if (action == "p") {
    _vm->printBindings();
  }
  else if (action == "m") {
    if (i > 3)
      _vm->printMem(args[1].toInt(), args[2].toInt(), true);
    else
      _vm->printMem(args[1].toInt(), args[2].toInt(), false);
  }
  else if (action == "s") {
    uint16_t steps = 1;
    if (i > 1) {
      steps = args[1].toInt();
    }
    for (uint16_t j = 0; j < steps; j++)
      _vm->step();
  }
  else if (action == "q") {
    _vm->printStack();
  }
  else if (action == "r") {
    _vm->printRegisters();
  }
  else if (action == "~") {
    _vm->printStatus();
  }
  else if (action == "c") {

    // c for "cast"
    String dm = args[1];
    uint16_t addr = args[2].toInt();
    dprint(F("mem["));
    dprint(String(addr) + "] ", static_cast<uint8_t>(PrintCategory::REPL));
    dprintln("as " + dm + ": " + _vm->getAsString(addr, dm), static_cast<uint8_t>(PrintCategory::REPL));

  }
  else if (action == "l") {
    uint16_t addr = args[1].toInt();
    String programStr = args[2];
    dprint(F("LOADING @ mem["));
    dprint(String(addr) + "] " + programStr + "\n", static_cast<uint8_t>(PrintCategory::REPL));
    loadProgram(programStr, addr);
  }
  else if (action == "i") {
    uint16_t newIP = args[1].toInt();    
    _vm->setIP(newIP);    
    dprint(F("IP = "));
    dprint(String(newIP), static_cast<uint8_t>(PrintCategory::REPL));    
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

