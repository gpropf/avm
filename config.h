#ifndef config_h
#define config_h


enum class PrintCategory : uint8_t {
  MATH = 0b00000001,
  PUSH = 0b00000010,
  POP = 0b00000100,
  MEM = 0b00001000,
  PRINT = 0b00010000,
  STATUS = 0b00100000,
  REPL = 0b01000000,
  REG = 0b10000000,
};

const uint8_t ANALOG_TEST_INPUT_PIN = A1;
const uint8_t ANALOG_OUT_PIN = 10;
const uint8_t BUTTON_INPUT_PIN = 2;
const uint8_t PIR_INPUT_PIN = 3;
const uint8_t NUM_PINS = 6;

const uint8_t READ_BUFFER_LENGTH = 5;
const uint8_t REPL_DELAY = 10;

const uint16_t VM_MEM_SIZE = 300;
const uint16_t STACK_TOP = VM_MEM_SIZE;
const uint16_t VM_STACK_SIZE = 50;
const uint16_t MAX_STRING_LENGTH = 1024;
const uint16_t FUNCTION_START = 35;
const uint16_t BAILOUT = 35;
const uint8_t REGISTER_BLOCK_SIZE = 128;

///*
const uint8_t categoriesToPrint = static_cast<uint8_t>(PrintCategory::POP) | static_cast<uint8_t>(PrintCategory::REG) |
                                  static_cast<uint8_t>(PrintCategory::REPL) | static_cast<uint8_t>(PrintCategory::STATUS) |
                                  static_cast<uint8_t>(PrintCategory::MEM) | static_cast<uint8_t>(PrintCategory::PUSH) |
                                  static_cast<uint8_t>(PrintCategory::MATH) | static_cast<uint8_t>(PrintCategory::PRINT);

//*/

//const uint8_t categoriesToPrint = static_cast<uint8_t>(PrintCategory::REPL) | static_cast<uint8_t>(PrintCategory::PRINT);
#endif
