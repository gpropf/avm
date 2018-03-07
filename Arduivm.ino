#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"
#include "REPL.h"


VM vm(VM_MEM_SIZE, VM_STACK_SIZE);

REPL repl("$[", "]:", &vm);


void setup() {

  Serial.begin(57600);
  //dprint(F("{{"));
  dprintln(repeatString("*", 60));

  // dprintln("Pin A0 is:" + String(A0));

  //dprintln(F("}}"));
  dprintln(F("Arduivm: v0.5.0"));
  dprintln(repeatString("*", 60));
  dprintln("Size of stackElement:" + String(sizeof(stackElement)));

  pinMode(ANALOG_OUT_PIN, OUTPUT);
  //pinMode(ANALOG_TEST_INPUT_PIN, INPUT);

  //uint8_t testRead = analogRead(ANALOG_TEST_INPUT_PIN);
  //dprintln("Testing analog input by reading pin " + String(ANALOG_TEST_INPUT_PIN) + ", got value: " + String(testRead));

  //dprintln("Testing LED, on pin " + String(ANALOG_OUT_PIN) + ", it should blink twice.");
  // put your setup code here, to run once:
  //Cell ** cells;
  //cells = new Cell*[10];
  //Number n = Number();
  for (uint8_t i = 0; i < 5; i++) {
    analogWrite(ANALOG_OUT_PIN, 255);
    delay(250);
    analogWrite(ANALOG_OUT_PIN, 0);
    delay(100);
  }
  analogWrite(ANALOG_OUT_PIN, 255);
  delay(500);
  analogWrite(ANALOG_OUT_PIN, 0);

  vm.writeInstruction(Opcode::BINDAI, static_cast<uint16_t>(20), ANALOG_TEST_INPUT_PIN);
  vm.writeInstruction(Opcode::PUSH, static_cast<uint16_t>(20));
  vm.writeInstruction(Opcode::PUSH, static_cast<uint16_t>(22));
  vm.writeInstruction(Opcode::ADD);
  vm.writeInstruction(Opcode::POP, static_cast<uint16_t>(30));
  
  vm.changeIP();
}

void loop() {
  dprintln("\n\nloop() starting....");
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
