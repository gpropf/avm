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
  dprintln(F("Arduivm: v0.13-cmake.0"), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(repeatString("*", 60), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln("END_8:" + String(static_cast<uint8_t>(Opcode::END_8)),
           static_cast<uint8_t>(PrintCategory::REPL));

  flashLEDs();

  const uint8_t program[] = {
#include "pow.h"
    65, 86, 77, 10, 0, 0, 0, 79, 0, 0, 84, 0
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
