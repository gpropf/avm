#include "Arduino.h"

#include "VM.h"

#include "Cell.h"
#include "Instructions.h"
#include "Number.h"

static const uint8_t TEST_INPUT_PIN = A1;

VM vm(20, 10);

void setup() {

  Serial.begin(57600);
  Serial.println(String(F("{{ ******************************{{==}} ****************************** }}")));
  Serial.println(String(F(" {{ ************* Arduivm: Virtually the Best VM Ever! ************* }}")));
  Serial.println(String(F("{{ ******************************{{==}} ****************************** }}")));



  pinMode(TEST_INPUT_PIN, INPUT);
  uint8_t testRead = analogRead(TEST_INPUT_PIN);
  Serial.println("Testing analog input by reading pin " + String(TEST_INPUT_PIN) + ", got value: " + String(testRead));
  // put your setup code here, to run once:
  //Cell ** cells;
  //cells = new Cell*[10];
  //Number n = Number();
  Int32 * i5 = new Int32(5);
  Int32 * i4 = new Int32(4);
  Int32 * i19 = new Int32(19);
  Int32 * i81 = new Int32(81);
  Int32 * zero = new Int32(0);
  Int32 * testPin = new Int32(TEST_INPUT_PIN);
  AnalogRead * ar = new AnalogRead();;
  Add2 * add2 = new Add2();
  JmpIfGreater * jig = new JmpIfGreater();
  //cells[0]=&n;
  //cells[0]->exec();
  Halt * halt = new Halt();
  vm.writeCell(i5, 0);
  vm.writeCell(i4, 1);
  vm.writeCell(add2, 2);
  vm.writeCell(zero, 3);

  vm.writeCell(i81, 4);

  vm.writeCell(testPin, 5);
  vm.writeCell(ar, 6);
  vm.writeCell(jig, 7);
  vm.writeCell(halt, 8);
  vm.writeCell(halt, 9);
}

void loop() {
  // put your main code here, to run repeatedly:
  for (uint16_t i = 0; i < 20; i++)
    vm.step();
  Serial.println("---");
  vm.reset();
  delay(50000);
}
