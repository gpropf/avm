#ifndef REPL_h
#define REPL_h

/* **************************************************
 *  REPL: This is meant to be a fairly generic REPL for any Arduino project
 *  where there might be a need for interaction with the project during its
 *  runtime.
 *  *************************************************
 */

class REPL {


  public:
    enum RunMode : uint8_t {
      STEP = 0,
      RUN = 1
    };

    enum Cmd: uint8_t {
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

    REPL(String leftPrompt, String rightPrompt, VM *vm): _leftPrompt(leftPrompt),
      _rightPrompt(rightPrompt),
      _vm(vm) {};

    String getPromptString(String subPrompt);
    String readCommand();
    //void evalCommand(String cmd);
    void loop(String subPrompt = "");
    VM * bindVM(VM * vm);
    void toggleStepping();
    void parseCommand(String s); 
    
  private:
    RunMode _runMode = STEP;
    boolean _steppingMode = true;
    String _leftPrompt, _rightPrompt;
    uint16_t cmdCount = 0;
    uint16_t eptr = 0;
    VM *_vm;
};



#endif
