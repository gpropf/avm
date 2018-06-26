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
  flashLEDs();
  dprintln(repeatString("*", 20), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(F("Arduivm: v0.15-cmake.0"), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(repeatString("*", 20), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln("END_8:" + String(static_cast<uint8_t>(Opcode::END_8)),
           static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(repeatString("~", 20), static_cast<uint8_t>(PrintCategory::REPL));
  
  const uint8_t program[] = {
//#include "test-math.h"
//#include "read-and-blink.h"
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
//  vm.reset();
}
