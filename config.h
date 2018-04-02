#ifndef config_h
#define config_h

const uint8_t ANALOG_TEST_INPUT_PIN = A1;
const uint8_t ANALOG_OUT_PIN = 10;
const uint8_t NUM_PINS = 20;

const uint8_t READ_BUFFER_LENGTH = 5;
const uint8_t REPL_DELAY = 10;

const uint16_t VM_MEM_SIZE = 300;
const uint16_t STACK_TOP = VM_MEM_SIZE;
const uint16_t VM_STACK_SIZE = 50;
const uint16_t MAX_STRING_LENGTH = 1024;
const uint16_t FUNCTION_START = 35;
const uint16_t BAILOUT = 35;
const uint8_t verbosityThreshold = 1; // 0 means ALWAYS print, anything higher has less importance

#endif
