#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"
#include "REPL.h"


VM vm(VM_MEM_SIZE, VM_STACK_SIZE);

REPL repl("$[", "]:", &vm);


void flashLEDs () {
  pinMode(ANALOG_OUT_PIN, OUTPUT);

  for (uint8_t i = 0; i < 5; i++) {
    analogWrite(ANALOG_OUT_PIN, 255);
    delay(250);
    analogWrite(ANALOG_OUT_PIN, 0);
    delay(100);
  }
  analogWrite(ANALOG_OUT_PIN, 255);
  delay(500);
  analogWrite(ANALOG_OUT_PIN, 0);
}

void setup() {

  Serial.begin(57600);

  dprintln(repeatString("*", 60));
  dprintln(F("Arduivm: v0.9.0"));
  dprintln(repeatString("*", 60));

  flashLEDs();


  uint8_t regTargets = 0x45;

  Opcode testop = static_cast<Opcode>(27);
  Opcode widePush = VM::getOpcodeByDataWidth (Opcode::PUSH_MEM_8, 2);

  OpcodeAndDataWidth opPair = VM::getOpcodeAndDataWidth(testop);
  dprintln("8 bit instructions end at " + String(static_cast<uint8_t>(Opcode::END_8)));
  dprintln("Test OP = " + String(static_cast<uint8_t>(opPair.c)) + ", Width = " + String(opPair.dw));

  vm.writeInstruction(Opcode::BINDAI, static_cast<uint16_t>(VM::DATA_SEG + ANALOG_TEST_INPUT_PIN), ANALOG_TEST_INPUT_PIN);
  vm.writeInstruction(widePush, static_cast<uint16_t>(VM::DATA_SEG));


  dprintln("Wide Push = " + String(static_cast<uint8_t>(widePush)));
  vm.writeInstruction(widePush, static_cast<uint16_t>(VM::DATA_SEG + 2));
  //vm.writeInstruction(Opcode::ADD);
  vm.writeInstruction(VM::getOpcodeByDataWidth (Opcode::POP_REGS_8, 2), 0, regTargets);
  //vm.writeInstruction(Opcode::DATA_FLOAT);
  vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 22));
  vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 26));
  //vm.writeInstruction(Opcode::ADD);
  regTargets = 0x89;
  vm.writeInstruction(Opcode::POP_REGS_8, 0, regTargets);
  //vm.writeInstruction(Opcode::REL_MODE);
  //vm.writeInstruction(Opcode::DATA_UINT8);
  //vm.writeInstruction(Opcode::DATA_STRING);
  vm.writeInstruction(Opcode::ADD_INT_8, 0, static_cast<uint8_t>(0x45));


  vm.writeInstruction(Opcode::CALL, static_cast<uint16_t>(30));

  // This block of NOOPs is just to mark the end of the program
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);

  vm.writeInstruction(Opcode::RET);
  vm.writeInstruction(Opcode::NOOP);

  /*
    vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 60));
    vm.writeInstruction(Opcode::POP_REGS_8, static_cast<uint16_t>(VM::DATA_SEG + 80));
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    //vm.writeInstruction(Opcode::REL_MODE);
    //vm.writeInstruction(Opcode::DATA_UINT16);
    vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 30));
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
  */

  /* **********************************************************************
      This code writes our initial constants into memory above the program
   * **********************************************************************
  */
  float const fl1 = 1.3;
  float const fl2 = 3.14159;
  float sumfls = fl1 + fl2;
  vm.writeData(fl1, VM::DATA_SEG + 22, false);
  vm.writeData(fl2, VM::DATA_SEG + 26, false);
  vm.writeData(sumfls, VM::DATA_SEG + 34, false);
  vm.writeData(static_cast<uint16_t>(VM::DATA_SEG + 40), VM::DATA_SEG + 30, false);
  //const String testStr = "Greg";
  //vm.writeString(testStr.c_str(), VM::DATA_SEG + 60, false);
  //vm.writeData(static_cast<const uint8_t>(0), VM::DATA_SEG + 60 + testStr.length(), false);

  // Call changeIP with no args to reset the counter
  vm.changeIP();
}

void loop() {
  //dprintln("\n\nloop() starting....");
  //while (true) {
  repl.loop();
  //}
  delay(1000);
  // put your main code here, to run repeatedly:
  for (uint16_t i = 0; i < 20; i++)
    vm.step();

  delay(50000);
  dprintln("---");
  vm.reset();
}
