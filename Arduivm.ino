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
  dprintln(F("Arduivm: v0.10.0"));
  dprintln(repeatString("*", 60));

  dprintln("MOV_SPREL2_REG_8:" + static_cast<uint8_t>(Opcode::MOV_SPREL2_REG_8));

  flashLEDs();

  //dprintln("Numeric value of comparisons 3/4, 4/3, 3/3: " + String (3 > 4) + "," + String (4 > 3) + "," + String (3 == 3));
  //dprintln("END_8 = " + String(static_cast<uint8_t>(Opcode::END_8)));


  uint8_t regTargets = 0xef;

  //Opcode testop = static_cast<Opcode>(27);
  Opcode widePush = VM::getOpcodeByDataWidth (Opcode::PUSH_MEM_8, 2);

  //OpcodeAndDataWidth opPair = VM::getOpcodeAndDataWidth(testop);
  dprintln("END_8:" + String(static_cast<uint8_t>(Opcode::END_8)));
  //dprintln("Test OP = " + String(static_cast<uint8_t>(opPair.c)) + ", Width = " + String(opPair.dw));

  vm.writeInstruction(Opcode::BINDAI, static_cast<uint16_t>(VM::DATA_SEG + ANALOG_TEST_INPUT_PIN), ANALOG_TEST_INPUT_PIN);
  vm.writeInstruction(widePush, static_cast<uint16_t>(VM::DATA_SEG));


  //dprintln("Wide Push = " + String(static_cast<uint8_t>(widePush)));
  vm.writeInstruction(widePush, static_cast<uint16_t>(VM::DATA_SEG + 2));
  vm.writeInstruction(VM::getOpcodeByDataWidth (Opcode::POP_REGS_8, 2), 0, regTargets);
  vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 22));
  vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 26));
  regTargets = 0x89;
  vm.writeInstruction(Opcode::POP_REGS_8, 0, regTargets);
  vm.writeInstruction(Opcode::ADD_INT_8, 0, static_cast<uint8_t>(0xef));
  vm.writeInstruction(Opcode::PUSH_CONST_8, 0, static_cast<uint8_t>(0x5));
  vm.writeInstruction(Opcode::PUSH_CONST_8, 0, static_cast<uint8_t>(0x3));


  vm.writeInstruction(Opcode::CALL, static_cast<uint16_t>(FUNCTION_START));

  // This block of NOOPs is just to mark the end of the program
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);

  // This is my test function: pow(x:u8, n:u8)
  // pseudocode is:
  /*

    uint8_t i = 0;
    uint8_t retval = 1;
    [LOOP_START]
    if (i == n ) goto [BAILOUT]
    i++;
    retval = retval * x;
    goto [LOOP_START]

    [BAILOUT]
    return retval;

  */
  regTargets = 0x89;
  vm.setIP(FUNCTION_START);
  //vm._ip16 = FUNCTION_START;
  vm.writeInstruction(Opcode::PUSH_CONST_8, 0, static_cast<uint8_t>(0x0));
  vm.writeInstruction(Opcode::PUSH_CONST_8, 0, static_cast<uint8_t>(0x1));
  vm.writeInstruction(Opcode::MOV_SPREL2_REG_8, 0, static_cast<uint8_t>(0x1), static_cast<uint8_t>(0x1));
  vm.writeInstruction(Opcode::MOV_SPREL2_REG_8, 0, static_cast<uint8_t>(0x4), static_cast<uint8_t>(0x4));
  regTargets = 0x14;
  vm.writeInstruction(Opcode::CMP_INT_8, 0, regTargets);
  vm.writeInstruction(Opcode::JEQ, static_cast<uint16_t>(FUNCTION_START + BAILOUT));
  //vm.writeInstruction(Opcode::NOOP); // FIXME: need JEQ [BAILOUT] here
  vm.writeInstruction(Opcode::INC_SPREL_UINT_8, 0, static_cast<uint8_t>(0x1));
  vm.writeInstruction(Opcode::MOV_SPREL2_REG_8, 0, static_cast<uint8_t>(0x0), static_cast<uint8_t>(0x0));
  vm.writeInstruction(Opcode::MOV_SPREL2_REG_8, 0, static_cast<uint8_t>(0x5), static_cast<uint8_t>(0x5));
  vm.writeInstruction(Opcode::MUL_INT_8, 0, static_cast<uint8_t>(0x05));
  vm.writeInstruction(Opcode::MOV_REG2_SPREL_8, 0, static_cast<uint8_t>(0x0), static_cast<uint8_t>(0x0));
  vm.writeInstruction(Opcode::UJMP, static_cast<uint16_t>(FUNCTION_START + 4));
  vm.writeInstruction(Opcode::NOOP);
  vm.writeInstruction(Opcode::NOOP);

  vm.writeInstruction(Opcode::NOOP);
  //vm.writeInstruction(Opcode::CMP_INT_8, 0, regTargets);
  regTargets = 0x67;
  vm.writeInstruction(Opcode::POP_REGS_8, 0, regTargets);
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
