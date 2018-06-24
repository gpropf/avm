// Simple C++ program to display "Hello World"

// Header file for input output functions
#include<iostream>
#include "Arduino.h"
//#include <cctype>
#include "config.h"
#include "util.h"

#include "VM.h"
#include "REPL.h"

using namespace std;

//#include "dummy_objects.h"
//FakeSerial Serial;

FakeSerial Serial;

VM vm(VM_MEM_SIZE, VM_STACK_SIZE);

REPL repl(String((char *)"x86["), String((char *)"]:"), &vm);

void x86setup() {
  dprintln(repeatString(F("*"), 20), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(F("Arduivm: v0.15-x86.0"), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(repeatString(F("*"), 20), static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(F("END_8:") + String(static_cast<uint8_t>(Opcode::END_8)),
           static_cast<uint8_t>(PrintCategory::REPL));
  dprintln(repeatString(F("~"), 20), static_cast<uint8_t>(PrintCategory::REPL));
  
   uint8_t program[] = {
#include "test-math.h"
    //#include "read-and-blink.h"
    //#include "pow.h"
    65, 86, 77, 10, 0, 0, 0, 79, 0, 0, 84, 0
  };
  uint8_t * srcptr = program;
  uint8_t * destptr = &vm._mem[0];
  //dprintln("Before program loading :" + String(vm.getIP()) + ")");
  vm.moveData(srcptr, destptr, sizeof(program));
}


// main function -
// where the execution of program begins
int main()
{
  // prints heelo world
  cout<<"Hello AVM World\n";

  x86setup();
  repl.loop();
  return 0;
}
