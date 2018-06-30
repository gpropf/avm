###### Python Instruction Codes Constant Block ######

## ----------------------------------- 	VARIABLE DATA WIDTH INSTRUCTIONS 	
MATH_BASE_8 = 0 + 0 ## Math Operations Base
ADD_UINT_8 = MATH_BASE_8 + 0 
SUB_UINT_8 = MATH_BASE_8 + 1 
MUL_UINT_8 = MATH_BASE_8 + 2 
DIV_UINT_8 = MATH_BASE_8 + 3 
ADD_INT_8 = MATH_BASE_8 + 4 
SUB_INT_8 = MATH_BASE_8 + 5 
MUL_INT_8 = MATH_BASE_8 + 6 
DIV_INT_8 = MATH_BASE_8 + 7 
OR_8 = MATH_BASE_8 + 8 
AND_8 = MATH_BASE_8 + 9 
NOT_8 = MATH_BASE_8 + 10 
SHL_8 = MATH_BASE_8 + 11 
SHR_8 = MATH_BASE_8 + 12 
MOV_BASE_8 = MATH_BASE_8 + 13 ## Move Operations Base
MOV_REG2_MEM_8 = MOV_BASE_8 + 0 
MOV_MEM2_REG_8 = MOV_BASE_8 + 1 
MOV_REG2_SPREL_8 = MOV_BASE_8 + 2 
MOV_SPREL2_REG_8 = MOV_BASE_8 + 3 
MOV_REG_IND2_SPREL_8 = MOV_BASE_8 + 4 
MOV_SPREL_IND2_REG_8 = MOV_BASE_8 + 5 
MOV_REG_IND2_REG_8 = MOV_BASE_8 + 6 
MOV_REG_IND2_REG_IND_8 = MOV_BASE_8 + 7 
MOV_REG2_REG_IND_8 = MOV_BASE_8 + 8 
MOV_REG2_REG_8 = MOV_BASE_8 + 9 
PUSH_BASE_8 = MOV_BASE_8 + 10 ## Push Operations Base
PUSH_MEM_8 = PUSH_BASE_8 + 0 
PUSH_SPREL_8 = PUSH_BASE_8 + 1 
PUSH_REGS_8 = PUSH_BASE_8 + 2 ##  push one or two registers [R4,R4], if both are the same register then push only that one.
PUSH_REGS_IND_8 = PUSH_BASE_8 + 3 
PUSH_CONST_8 = PUSH_BASE_8 + 4 
POP_BASE_8 = PUSH_BASE_8 + 5 ## Pop Operations Base
POP_REGS_8 = POP_BASE_8 + 0 ## Same as push, if there are two differe nt regs this pops 2 values.
CMP_BASE = POP_BASE_8 + 1 ## Comparison Operations Base
CMP_INT_8 = CMP_BASE + 0 
CMP_UINT_8 = CMP_BASE + 1 
INC_BASE_spacer = CMP_BASE + 2 ## Inc Operations Base
INC_BASE = CMP_BASE + 3 ## Inc Operations Base
INC_SPREL_UINT_8 = INC_BASE + 0 
INC_SPREL_INT_8 = INC_BASE + 1 
INC_END = INC_SPREL_INT_8 + 0 
END_8 = INC_END + 0 

## ----------------------------------- 	FIXED DATA WIDTH INSTRUCTIONS 	
FIXED_WIDTH_BASE = 200 + 0 
CMP_FLOAT = FIXED_WIDTH_BASE + 0 
CMP_STRING = FIXED_WIDTH_BASE + 1 
JUMP_BASE = FIXED_WIDTH_BASE + 2 ## Jump Operations Base
JNE = JUMP_BASE + 0 
JEQ = JUMP_BASE + 1 
JLT = JUMP_BASE + 2 
JGT = JUMP_BASE + 3 
UJMP = JUMP_BASE + 4 ##  Unconditional jump
BIND_BASE = JUMP_BASE + 5 ## Bind Operations Base
BINDAI = BIND_BASE + 0 ## A16 V8: Bind a mem address (uint16_t) (analog input) to a pin (uint8_t)
BINDDI = BIND_BASE + 1 ## A16 V8: Bind a mem address (uint16_t) (digital input) to a pin (uint8_t)
BINDAO = BIND_BASE + 2 ## A16 V8: Bind a mem address (uint16_t) (analog out) to a pin (uint8_t)
BINDDO = BIND_BASE + 3 ## A16 V8: Bind a mem address (uint16_t) (digital output) to a pin (uint8_t)
BINDAP = BIND_BASE + 4 ## A16 V8: Bind a mem address (uint16_t) (analog input-pullup) to a pin (uint8_t)
BINDDP = BIND_BASE + 5 ## A16 V8: Bind a mem address (uint16_t) (digital input-pullup) to a pin (uint8_t)
SP_ADJ = BIND_BASE + 6 ##  increment the SP without pop. Takes uint8_t as arg.
PRINT_AS = BIND_BASE + 7 ##  Takes an 8 bit operand. First nibble is register to print, 2nd is type to print as.
FLOAT_MATH_BASE = BIND_BASE + 8 ## Floating point math Operations Base
ADD_FL = FLOAT_MATH_BASE + 0 
SUB_FL = FLOAT_MATH_BASE + 1 
MUL_FL = FLOAT_MATH_BASE + 2 
DIV_FL = FLOAT_MATH_BASE + 3 
SPAWNVM = FLOAT_MATH_BASE + 4 ##  Creates a new VM with arg1, arg2, and arg3 as the memory base address, the stack base, and the register base respectively
TOP_BASE = 249 + 0 ## Bind Operations Base
NOOP = TOP_BASE + 0 ## Top Operations Base
NOOP_INIT = TOP_BASE + 1 
CALL = 255 + 0 ##  Takes a uint16_t address of the function to call. Automatically saves return address
RET = 254 + 0 ##  Uses stored return address and leaves return value on stack
### END Python Constants ###


### Begin Python Metadata ###

## ----------------------------------- 	FIXED DATA WIDTH INSTRUCTIONS 	
mdFixedWidth = {'JEQ': {'opcode': 203, 'argFormats': ['H']}, 'UJMP': {'opcode': 206, 'argFormats': ['H']}, 'BINDAI': {'opcode': 207, 'argFormats': ['H', 'B']}, 'BINDDI': {'opcode': 208, 'argFormats': ['H', 'B']}, 'BINDAO': {'opcode': 209, 'argFormats': ['H', 'B']}, 'PRINT_AS': {'opcode': 214, 'argFormats': ['N', 'N']}, 'ADD_FL': {'opcode': 215, 'argFormats': ['N', 'N']}, 'SUB_FL': {'opcode': 216, 'argFormats': ['N', 'N']}, 'MUL_FL': {'opcode': 217, 'argFormats': ['N', 'N']}, 'DIV_FL': {'opcode': 218, 'argFormats': ['N', 'N']}, 'SPAWNVM': {'opcode': 219, 'argFormats': ['H', 'H', 'B']}, 'NOOP': {'opcode': 249, 'argFormats': []}, 'NOOP_INIT': {'opcode': 250, 'argFormats': []}, 'CALL': {'opcode': 255, 'argFormats': ['H']}, 'RET': {'opcode': 254, 'argFormats': []}}
## ----------------------------------- 	VARIABLE DATA WIDTH INSTRUCTIONS 	
mdMultiWidth = {'ADD_UINT_8': {'opcode': 0, 'argFormats': ['N', 'N']}, 'SUB_UINT_8': {'opcode': 1, 'argFormats': ['N', 'N']}, 'MUL_UINT_8': {'opcode': 2, 'argFormats': ['N', 'N']}, 'DIV_UINT_8': {'opcode': 3, 'argFormats': ['N', 'N']}, 'ADD_INT_8': {'opcode': 4, 'argFormats': ['N', 'N']}, 'MOV_REG2_MEM_8': {'opcode': 13, 'argFormats': ['B', 'H']}, 'MOV_MEM2_REG_8': {'opcode': 14, 'argFormats': ['H', 'B']}, 'MOV_REG2_SPREL_8': {'opcode': 15, 'argFormats': ['B', 'B']}, 'MOV_SPREL2_REG_8': {'opcode': 16, 'argFormats': ['B', 'B']}, 'PUSH_MEM_8': {'opcode': 23, 'argFormats': ['H']}, 'PUSH_CONST_8': {'opcode': 27, 'argFormats': ['V']}, 'POP_REGS_8': {'opcode': 28, 'argFormats': ['N', 'N']}, 'CMP_INT_8': {'opcode': 29, 'argFormats': ['N', 'N']}, 'INC_SPREL_UINT_8': {'opcode': 32, 'argFormats': ['B']}}

### End Python Metadata ###
