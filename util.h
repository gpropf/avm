String repeatString(const String s, const uint8_t n);
//void dprint(String s);
//void dprintln(String s);
//inline void dprintln(String s, const uint8_t verbosity = 0) {};

void zeros(uint8_t * z, uint8_t nz);

const uint8_t NOT_BOUND = 255;


inline void dprintln(String s, const uint8_t verbosity = 0) {
  if (verbosity <= verbosityThreshold) {   
    Serial.flush();
    Serial.println(s);
    Serial.flush();
  }
}


inline void dprint(String s, const uint8_t verbosity = 0) {
  if (verbosity <= verbosityThreshold) {
    Serial.flush();
    Serial.print(s);
    Serial.flush();
  }
}
