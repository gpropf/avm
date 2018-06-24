
#include <Arduino.h>
#include <iostream>
#include <string>

using namespace std;

FakeString::FakeString(char* cptr) {
  this->_s = std::string(cptr);
}

FakeString::FakeString(std::string s) {
  this->_s = s;
}

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

bool FakeString::operator!=(const FakeString& rhs) {
  if (_s.compare(rhs._s) != 0)
    return true;
  return false;
}

FakeString FakeString::operator+(const char * cptr) {
  std::string rhsStr = std::string(cptr);
  return FakeString(_s + rhsStr);
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
uint8_t FakeSerial::readBytes(char *, int) {
  return 0;
}
uint8_t FakeSerial::readBytes() {
  return 0;
}
bool FakeSerial::available() { return true; }

ostream& operator<<(ostream& os, const FakeString& fs)  
{  
  os << fs._s;
  return os;  
}  
