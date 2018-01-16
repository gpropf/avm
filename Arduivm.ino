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
  dprintln(F("Arduivm: v0.3"));
  dprintln(repeatString("*", 60));

  pinMode(ANALOG_OUT_PIN, OUTPUT);
  //pinMode(ANALOG_TEST_INPUT_PIN, INPUT);

  //uint8_t testRead = analogRead(ANALOG_TEST_INPUT_PIN);
  //dprintln("Testing analog input by reading pin " + String(ANALOG_TEST_INPUT_PIN) + ", got value: " + String(testRead));

  //dprintln("Testing LED, on pin " + String(ANALOG_OUT_PIN) + ", it should blink twice.");
  // put your setup code here, to run once:
  //Cell ** cells;
  //cells = new Cell*[10];
  //Number n = Number();

  analogWrite(ANALOG_OUT_PIN, 255);
  delay(1000);
  analogWrite(ANALOG_OUT_PIN, 0);
  delay(1000);
  analogWrite(ANALOG_OUT_PIN, 255);
  delay(500);
  analogWrite(ANALOG_OUT_PIN, 0);

  int32_t intval1 = 1563535288;
  int32_t intval2 = -14350008;
  dprintln("Writing 2 32 bit int vals," + String(intval1) + " and " + String(intval2));
//  vm.writeData(intval1, 0);
//  vm.writeData(intval2, 4);
//  vm.changeIP();
//  int32_t i = vm.readData <int32_t> (0);

//  dprintln("Reading 32 bit int val: " + String(i));
//  i = vm.readData <int32_t> (4);

//  dprintln("Reading 32 bit int val: " + String(i));
  vm.writeInstruction(Opcode::BINDAI, ANALOG_TEST_INPUT_PIN, static_cast<uint16_t>(20));
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
