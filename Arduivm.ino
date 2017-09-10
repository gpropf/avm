#include "Arduino.h"

#include "VM.h"

#include "Cell.h"
#include "Instructions.h"
#include "Number.h"

static const uint8_t TEST_INPUT_PIN = A1;
static const uint8_t ANALOG_OUT_PIN = 10;

VM vm(20, 10);

void setup() {

  Serial.begin(57600);
  Serial.println(String(F("{{ ******************************{{==}} ****************************** }}")));
  Serial.println(String(F(" {{ ************* Arduivm: Virtually the Best VM Ever! ************* }}")));
  Serial.println(String(F("{{ ******************************{{==}} ****************************** }}")));

  pinMode(ANALOG_OUT_PIN, OUTPUT);
  pinMode(TEST_INPUT_PIN, INPUT);

  uint8_t testRead = analogRead(TEST_INPUT_PIN);
  Serial.println("Testing analog input by reading pin " + String(TEST_INPUT_PIN) + ", got value: " + String(testRead));

  Serial.println("Testing LED, on pin " + String(ANALOG_OUT_PIN) + ", it should blink twice.");
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
  Int32 * maxint8 = new Int32(255);
  Int32 * i7 = new Int32(7);
  Int32 * i3 = new Int32(3);
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
  vm.appendCell(maxint8);

  vm.appendCell(i3);
  vm.appendCell(i7);
  vm.appendCell(add2);
  vm.appendCell(aw1);
  vm.appendCell(i2500);

  vm.appendCell(dly);
  vm.appendCell(zero);
  vm.appendCell(i3);
  vm.appendCell(i7);
  vm.appendCell(add2);
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
