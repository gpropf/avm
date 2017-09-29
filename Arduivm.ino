#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"
#include "REPL.h"


VM vm(300, 10);

REPL repl("$[", "]:", &vm);


void setup() {

  Serial.begin(57600);
  //dprint(F("{{"));
  dprintln(repeatString("*", 60));

 // dprintln("Pin A0 is:" + String(A0));

  //dprintln(F("}}"));
  dprintln(F("Arduivm: v0.1"));
  dprintln(repeatString("*", 60));

  pinMode(ANALOG_OUT_PIN, OUTPUT);
  //pinMode(TEST_INPUT_PIN, INPUT);

  //uint8_t testRead = analogRead(TEST_INPUT_PIN);
  //dprintln("Testing analog input by reading pin " + String(TEST_INPUT_PIN) + ", got value: " + String(testRead));

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
