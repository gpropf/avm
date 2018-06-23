String repeatString(const String s, const uint8_t n);
//void dprint(String s);
//void dprintln(String s);
//inline void dprintln(String s, const uint8_t verbosity = 0) {};

void zeros(uint8_t * z, uint8_t nz);

const uint8_t NOT_BOUND = 255;


/*
void dprint(const char * sptr, const uint8_t pc = static_cast<uint8_t>(PrintCategory::STATUS)) {
  String s = String(sptr);
  dprint(s, pc);
}
*/

inline void dprintln(String s, const uint8_t pc = static_cast<uint8_t>(PrintCategory::STATUS)) {
  uint8_t cats = pc & categoriesToPrint;
  if (cats) {
    Serial.flush();
    // Serial.print("CATS:categoriesToPrint:" + String(cats) + "::" + String(categoriesToPrint) + ": ");
    Serial.println(s);

    Serial.flush();
  }
}


inline void dprint(String s, const uint8_t pc = static_cast<uint8_t>(PrintCategory::STATUS)) {
  uint8_t cats = pc & categoriesToPrint;
  if (cats) {
    Serial.flush();
    //Serial.print("CATS:categoriesToPrint:" + String(cats) + "::" + String(categoriesToPrint) + ">>> ");
    Serial.print(s);
    Serial.flush();
  }
}
