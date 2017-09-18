#include "Arduino.h"
#include "config.h"
#include "util.h"
#include "VM.h"

#include "Cell.h"
#include "Instructions.h"
#include "Number.h"



VM vm(20, 10);

void setup() {

  Serial.begin(57600);
  Serial.print(F("{{"));
  Serial.print(repeatString("*", 60));
  Serial.println(F("}}"));
  Serial.println(F("Arduivm: Virtually the Best VM Ever!"));


  //pinMode(ANALOG_OUT_PIN, OUTPUT);
  //pinMode(TEST_INPUT_PIN, INPUT);

  //uint8_t testRead = analogRead(TEST_INPUT_PIN);
  //Serial.println("Testing analog input by reading pin " + String(TEST_INPUT_PIN) + ", got value: " + String(testRead));

  //Serial.println("Testing LED, on pin " + String(ANALOG_OUT_PIN) + ", it should blink twice.");
  // put your setup code here, to run once:
  //Cell ** cells;
  //cells = new Cell*[10];
  //Number n = Number();
  /*
    analogWrite(ANALOG_OUT_PIN, 255);
    delay(1000);
    analogWrite(ANALOG_OUT_PIN, 0);
    delay(1000);
    analogWrite(ANALOG_OUT_PIN, 255);
    delay(500);
    analogWrite(ANALOG_OUT_PIN, 0);
  */
  Int32 * modeOutput = new Int32(OUTPUT);
  //Serial.println("Type of modeOutput: " + typeid(*modeOutput).name());
  Int32 * modeInput = new Int32(INPUT);
  PinMode * pm = new PinMode();
  Int32 * maxint8 = new Int32(255);
  Int32 * i7 = new Int32(7);
  Int32 * i3 = new Int32(3);
  Int32 * i10 = new Int32(10);
  Int32 * i19 = new Int32(19);
  Int32 * i81 = new Int32(81);
  Int32 * zero = new Int32(0);
  Int32 * testPin = new Int32(TEST_INPUT_PIN);
  AnalogRead * ar1 = new AnalogRead();
  Add2 * add2 = new Add2();
  JmpIfGreater * jig = new JmpIfGreater();
  AnalogWrite * aw1 = new AnalogWrite();
  Delay * dly = new Delay();
  Int32 * i2500 = new Int32(2500);

  //cells[0]=&n;
  //cells[0]->exec();
  Halt * halt = new Halt();
  //

  vm.appendCell(modeOutput);
  vm.appendCell(i10);
  vm.appendCell(pm);

  vm.appendCell(maxint8);
  vm.appendCell(i10);
  vm.appendCell(aw1);
  vm.appendCell(i2500);

  vm.appendCell(dly);
  vm.appendCell(zero);
  vm.appendCell(i10);
  vm.appendCell(aw1);
  /*
    vm.writeCell(i7, 0);
    vm.writeCell(i3, 1);
    vm.writeCell(add2, 2);
    vm.writeCell(zero, 3);
    vm.writeCell(maxint8, 4);
    vm.writeCell(i81, 5);

    vm.writeCell(testPin, 6);
    vm.writeCell(ar, 7);
    vm.writeCell(jig, 8);
    vm.writeCell(halt, 9);
  */
  vm.appendCell(halt);

  vm.appendCell(halt);

}

void loop() {
  Serial.println("\n\nloop() starting....");
  delay(1000);
  // put your main code here, to run repeatedly:
  for (uint16_t i = 0; i < 20; i++)
    vm.step();

  delay(50000);
  Serial.println("---");
  vm.reset();
}
