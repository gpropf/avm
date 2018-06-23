//#include "avr/variants/standard/pins_arduino.h"
#ifndef Arduino_h
#define Arduino_h

#include <stdint.h>
#include <string>



struct FakeString {
  
private:
  std::string _s;
  
public:
  uint8_t charAt(int);
  bool operator==(const FakeString& rhs);
  bool operator!=(const FakeString& rhs);
  bool operator!=(const char [1]);
  bool operator==(const char *);
  FakeString operator+=(const FakeString& rhs);
  FakeString operator+(const char *);
  
  FakeString operator+(const FakeString& rhs);
  //  char * operator+(const FakeString& rhs);
  //operator+=(FakeString lhs,  FakeString rhs);
  //FakeString operator+(const FakeString& rhs);
  FakeString(char*);
  FakeString(uint16_t);
  FakeString();


  int toInt();
  FakeString substring(int,int);
  FakeString substring(int);
  int length();
  int indexOf(const char *);
  
};

const char * operator+(const char *, const FakeString& rhs);
//const char * operator+(const char *, const char *);
const char * operator+(const FakeString& rhs, const char *);

typedef FakeString String;
//typedef String F;

#define F(s) String((char *)s)

typedef bool boolean;

const uint8_t A1 = 15;
const uint8_t INPUT = 0;
const uint8_t OUTPUT = 1;
const uint8_t INPUT_PULLUP = 2;

struct FakeSerial {
  // dummy class for using Arduino code with instances of Serial in them from cmake version


  
public:
  FakeSerial() {}
  static void flush();
  static void println(String s);
  static void print(String s);
  uint8_t readBytes(char *, int);
  uint8_t readBytes();
  bool available();

};

void delay(int);
void pinMode(uint8_t, uint8_t);
void analogWrite(uint8_t, uint8_t);
uint16_t analogRead(uint8_t);

void digitalWrite(uint8_t, uint8_t);
bool digitalRead(uint8_t);

extern FakeSerial Serial;

#endif
