
#include <Arduino.h>
#include <iostream>
#include <cstring>
#include <string>

using namespace std;

FakeString::FakeString(char* cptr) {
  this->_s = std::string(cptr);
}

FakeString::FakeString(std::string s) {
  this->_s = s;
}

FakeString::FakeString() {}
/*
FakeString::FakeString(uint16_t u16) {
  _s = std::to_string(u16);
}

FakeString::FakeString(uint8_t u8) {
  _s = std::to_string(u8);
}

FakeString::FakeString(uint32_t u32) {
  _s = std::to_string(u32);
}
*/

bool FakeString::operator==(const FakeString rhs) {
  if (_s.compare(rhs._s) == 0)
    return true;
  return false;
}

bool operator==(const FakeString lhs, const FakeString rhs) {
  if (lhs._s.compare(rhs._s) == 0)
    return true;
  return false;
}

bool FakeString::operator==(const char *) {
  return false;
}

bool FakeString::operator!=(const char *) {
  return false;
}

bool FakeString::operator!=(const FakeString& rhs) {
  if (_s.compare(rhs._s) != 0)
    return true;
  return false;
}

FakeString FakeString::operator+(const char * cptr) {
  std::string rhsStr = std::string(cptr);
  return FakeString(_s + rhsStr);
}

FakeString FakeString::operator+=(const FakeString& rhs) {
  _s += rhs._s;
  return _s;
}
  
FakeString FakeString::operator+(const FakeString rhs) {
  
  return FakeString(_s + rhs._s);
}



void FakeSerial::flush() {}
void FakeSerial::println(String s) {
  print(s);
  cout << '\n';
}
void FakeSerial::print(String s) {
  cout << s;
}
uint8_t FakeSerial::readBytes(char * buf, int numBytes) {
  cout << "Looking for " << std::to_string(numBytes) << " bytes of input...\n";
  cin >> buf;
  cout << "Read input: " << buf << "\n";
  return numBytes;
}
uint8_t FakeSerial::readBytes() {
  return 0;
}
bool FakeSerial::available() { return true; }



void  analogWrite(uint8_t, uint8_t) {}
uint16_t  analogRead(uint8_t) { return 4242; }

void digitalWrite(uint8_t, uint8_t) {}
bool digitalRead(uint8_t) { return true; }

void delay(int) {}
void pinMode(uint8_t, uint8_t) {}



ostream& operator<<(ostream& os, const FakeString& fs)  
{  
  os << fs._s;
  return os;  
}  
