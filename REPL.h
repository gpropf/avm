#ifndef REPL_h
#define REPL_h



class REPL {
    String _leftPrompt, _rightPrompt;
    uint16_t cmdCount = 0;
    uint16_t eptr = 0;
    VM *_vm;

  public:
    enum RunMode : uint8_t {
      STEP = 0,
      RUN = 1
    };
  private:
    RunMode _runMode = STEP;
    boolean _steppingMode = true;


  public:
    inline RunMode getMode() const {
      return _runMode;
    };
    REPL(String leftPrompt, String rightPrompt, VM *vm): _leftPrompt(leftPrompt),
      _rightPrompt(rightPrompt),
      _vm(vm) {};

    String getPromptString(String subPrompt);
    String readCommand();
    void evalCommand(String cmd);
    void loop(String subPrompt = "");
    VM * bindVM(VM * vm);
    void toggleStepping();

};



#endif
