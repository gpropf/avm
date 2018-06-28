#ifndef REPL_h
#define REPL_h

/* **************************************************
    REPL: This is meant to be a fairly generic REPL for any Arduino project
    where there might be a need for interaction with the project during its
    runtime.
 *  *************************************************
*/

class REPL {


  public:
    enum RunMode : uint8_t {
      STEP = 0,
      RUN = 1
    };

    enum Cmd : uint8_t {
      NOCMD = 0,
      BINDPIN = 1,
      PRINTMEM = 2,
    };

    inline RunMode getMode() const {
      return _runMode;
    };

    union Arg {
      int i;
      char * c;
      float f;
    };

    REPL(String leftPrompt, String rightPrompt, VM *vm, VM *vm2): _leftPrompt(leftPrompt),
      _rightPrompt(rightPrompt),
      _vm(vm),
      _vm2(vm2) {};

    String getPromptString(String subPrompt);
    String readCommand();
    //void evalCommand(String cmd);
    void loop(String subPrompt = String((char *)""));
    VM * bindVM(VM * vm);
    void toggleStepping();
    void parseCommand(String s);

  private:
    RunMode _runMode = STEP;
    boolean _steppingMode = true;
    String _leftPrompt, _rightPrompt;
    uint16_t cmdCount = 0;
    uint16_t _eptr = 0;
    VM *_vm;
    VM *_vm2;

    void loadProgram(String programStr, uint16_t addr);
    
    uint8_t hexToDec(char hex);
};



#endif
