#include "Arduino.h"
#include "config.h"
#include "util.h"

String repeatString(const String s, const uint8_t n) {
  String rs = "";
  for (uint8_t i = 0; i < n; i++) {
    rs += s;
  }
  return rs;
}

void dprint(String s) {
  Serial.flush();
  Serial.print(s);
  Serial.flush();
}

void dprintln(String s) {
  Serial.flush();
  Serial.println(s);
  Serial.flush();
}

void zeros(uint8_t * z, uint8_t nz) {
  for (uint8_t i = 0; i < nz; i++)
    z[i] = 0;
}

