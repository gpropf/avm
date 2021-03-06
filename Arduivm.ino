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

  /*
    pinMode(BUTTON_INPUT_PIN, INPUT);

      int val = 0;
      while (true) {
        val = digitalRead(BUTTON_INPUT_PIN);
        dprintln("Input Pin: " + String(val),1);
      }
  */
}

void testBind() {
  vm.writeInstruction(Opcode::BINDAI, static_cast<uint16_t>(VM::DATA_SEG + ANALOG_TEST_INPUT_PIN), ANALOG_TEST_INPUT_PIN);
}


void testAdd(DataMode dm) {
  const uint8_t regTargets = 0xef;
  Opcode widePush = VM::getOpcodeByDataWidth (Opcode::PUSH_MEM_8, 2);


  vm.writeInstruction(widePush, static_cast<uint16_t>(VM::DATA_SEG));
  vm.writeInstruction(widePush, static_cast<uint16_t>(VM::DATA_SEG + 2));
  vm.writeInstruction(VM::getOpcodeByDataWidth (Opcode::POP_REGS_8, 2), 0, regTargets);
  vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 22));
  vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 26));



  vm.writeInstruction(Opcode::POP_REGS_8, 0, regTargets);
  vm.writeInstruction(Opcode::ADD_UINT_8, 0, static_cast<uint8_t>(regTargets));
}


void setup() {

  Serial.begin(57600);

  dprintln(repeatString("*", 60), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(F("Arduivm: v0.11.0"), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(repeatString("*", 60), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln("END_8:" + String(static_cast<uint8_t>(Opcode::END_8)),
           static_cast<uint8_t>(PrintCategory::REPL));

  

  flashLEDs();
  /*
    uint8_t regTargets = 0xef;
    vm.writeInstruction(Opcode::PUSH_CONST_8, 0, static_cast<uint8_t>(0x5));
    vm.writeInstruction(Opcode::PUSH_CONST_8, 0, static_cast<uint8_t>(0x3));
    testBind();
    vm.writeInstruction(Opcode::CALL, static_cast<uint16_t>(FUNCTION_START));
    // This block of NOOPs is just to mark the end of the program
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
  */

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
  /*
    vm.setIP(FUNCTION_START);
    regTargets = 0x00;
    Opcode popAddr = VM::getOpcodeByDataWidth (Opcode::POP_REGS_8, 2);
    vm.writeInstruction(popAddr, 0, regTargets);



    vm.writeInstruction(Opcode::PUSH_CONST_8, 0, static_cast<uint8_t>(0x0));
    vm.writeInstruction(Opcode::PUSH_CONST_8, 0, static_cast<uint8_t>(0x1));
    vm.writeInstruction(Opcode::MOV_SPREL2_REG_8, 0, static_cast<uint8_t>(0x1), static_cast<uint8_t>(0x1));
    vm.writeInstruction(Opcode::MOV_SPREL2_REG_8, 0, static_cast<uint8_t>(0x2), static_cast<uint8_t>(0x4));

    regTargets = 0x14;
    vm.writeInstruction(Opcode::CMP_INT_8, 0, regTargets);
    vm.writeInstruction(Opcode::JEQ, static_cast<uint16_t>(FUNCTION_START + BAILOUT));
    vm.writeInstruction(Opcode::INC_SPREL_UINT_8, 0, static_cast<uint8_t>(0x1));
    vm.writeInstruction(Opcode::MOV_SPREL2_REG_8, 0, static_cast<uint8_t>(0x0), static_cast<uint8_t>(0x2));
    vm.writeInstruction(Opcode::MOV_SPREL2_REG_8, 0, static_cast<uint8_t>(0x3), static_cast<uint8_t>(0x5));

    regTargets = 0x25;
    vm.writeInstruction(Opcode::MUL_UINT_8, 0, regTargets);
    vm.writeInstruction(Opcode::MOV_REG2_SPREL_8, 0, static_cast<uint8_t>(0x2), static_cast<uint8_t>(0x0));
    vm.writeInstruction(Opcode::UJMP, static_cast<uint16_t>(FUNCTION_START + 6));
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::RET);
    vm.writeInstruction(Opcode::NOOP);


    vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 60));
    vm.writeInstruction(Opcode::POP_REGS_8, static_cast<uint16_t>(VM::DATA_SEG + 80));
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);
    //vm.writeInstruction(Opcode::REL_MODE);
    //vm.writeInstruction(Opcode::DATA_UINT16);
    vm.writeInstruction(Opcode::PUSH_MEM_8, static_cast<uint16_t>(VM::DATA_SEG + 30));
    vm.writeInstruction(Opcode::NOOP);
    vm.writeInstruction(Opcode::NOOP);


    float const fl1 = 1.3;
    float const fl2 = 3.14159;
    float sumfls = fl1 + fl2;
    vm.writeData(fl1, VM::DATA_SEG + 22, false);
    vm.writeData(fl2, VM::DATA_SEG + 26, false);
    vm.writeData(sumfls, VM::DATA_SEG + 34, false);
    vm.writeData(static_cast<uint16_t>(VM::DATA_SEG + 40), VM::DATA_SEG + 30, false);

    // Call changeIP with no args to reset the counter
    vm.changeIP();
  */
  const uint8_t program[] = {
#include "pow.h"
65,86,77,10,0,0,0,79,0,0
  };



  uint8_t * srcptr = program;
  uint8_t * destptr = &vm._mem[0];
  //dprintln("Before program loading :" + String(vm.getIP()) + ")");
  vm.moveData(srcptr, destptr, sizeof(program));
}

void loop() {
  repl.loop();
  vm.reset();
}
