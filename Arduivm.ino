#include "Arduino.h"

#include "VM.h"

#include "Cell.h"
#include "Instructions.h"
#include "Number.h"

VM vm(20, 10);

void setup() {
  Serial.begin(57600);
  // put your setup code here, to run once:
  //Cell ** cells;
  //cells = new Cell*[10];
  //Number n = Number();
  Int32 i5(5);
  Int32 i4(4);
  Add2 add2;
  //cells[0]=&n;
  //cells[0]->exec();
  vm.writeCell(&i5, 0);
  vm.writeCell(&i4, 1);
  vm.writeCell(&add2, 2);
  vm.step();
  vm.step();
  vm.step();
  vm.step();

}

void loop() {
  // put your main code here, to run repeatedly:

}
