
import re

import struct, itertools

class RefTranslator:
    def __init__(self, patternHash):
        self.patternHash = patternHash


    def translate(self,reftext,formatCode):
        for pattern,conversionFunc in self.patternHash.items():
            if pattern.match(reftext):
                return self.patternHash[pattern](reftext,formatCode)
        refval = labelRefs[reftext]
        bytes = struct.pack(formatCode,refval)
        return [byte for byte in bytes]


def getNibbles(text,formatCode):
    na,nb = map(lambda n: int(n), text.split(","))
    return [na * 16 + nb]
    
def getFloat(text,formatCode):
    bytes = struct.pack(formatCode,float(text))
    return [byte for byte in bytes]

def getInt(text, formatCode):
    bytes = struct.pack(formatCode,int(text))
    return [byte for byte in bytes]
    

pnibbles = re.compile("[0-9]+,[0-9]+")
pfloat = re.compile("[0-9]+\.[0-9]*")
pint = re.compile("[0-9]+")

ph = { pnibbles:getNibbles, pfloat:getFloat, pint:getInt}



translator = RefTranslator(ph)


MATH_BASE_8 = 0
ADD_UINT_8 = MATH_BASE_8
SUB_INT_8 = MATH_BASE_8 + 1
MUL_UINT_8 = MATH_BASE_8 + 2
DIV_INT_8 = MATH_BASE_8 + 3
OR_8   = MATH_BASE_8 + 4
AND_8 = MATH_BASE_8 + 5
NOT_8 = MATH_BASE_8 + 6
SHL_8 = MATH_BASE_8 + 7
SHR_8 = MATH_BASE_8 + 8

MOV_BASE_8 = MATH_BASE_8 + 9
MOV_REG2_MEM_8 = MOV_BASE_8
MOV_MEM2_REG_8 = MOV_BASE_8 + 1
MOV_REG2_SPREL_8 = MOV_BASE_8 + 2
MOV_SPREL2_REG_8 = MOV_BASE_8 + 3
MOV_REG_IND2_SPREL_8 = MOV_BASE_8 + 4
MOV_SPREL_IND2_REG_8 = MOV_BASE_8 + 5

MOV_REG_IND2_REG_8 = MOV_BASE_8 + 6
MOV_REG_IND2_REG_IND_8 = MOV_BASE_8 + 7
MOV_REG2_REG_IND_8 = MOV_BASE_8 + 8
MOV_REG2_REG_8 = MOV_BASE_8 + 9

PUSH_BASE_8 = MOV_BASE_8 + 10
PP_START_8 = PUSH_BASE_8
PUSH_MEM_8 = PUSH_BASE_8
PUSH_SPREL_8 = PUSH_BASE_8 + 1
PUSH_REGS_8 = PUSH_BASE_8 + 2 
PUSH_REGS_IND_8 = PUSH_BASE_8 + 3
PUSH_CONST_8 = PUSH_BASE_8 + 4

POP_BASE_8 = PUSH_BASE_8 + 5
POP_REGS_8 = POP_BASE_8 
PP_END_8 = POP_REGS_8

CMP_BASE = PP_END_8 + 1
CMP_INT_8 = CMP_BASE
CMP_UINT_8 = CMP_BASE + 1

INC_BASE = CMP_UINT_8 + 1
INC_SPREL_UINT_8 = INC_BASE + 1
INC_SPREL_INT_8 = INC_BASE + 2

INC_END = INC_SPREL_INT_8

END_8 = INC_END

FIXED_WIDTH_BASE = 200
CMP_FLOAT = FIXED_WIDTH_BASE
CMP_STRING = FIXED_WIDTH_BASE + 1

JUMP_BASE = FIXED_WIDTH_BASE + 2
JNE = JUMP_BASE
JEQ = JUMP_BASE + 1
JLT = JUMP_BASE + 2
JGT = JUMP_BASE + 3
UJMP = JUMP_BASE + 4

BIND_BASE = JUMP_BASE + 5
BINDAI = BIND_BASE
BINDDI = BIND_BASE + 1
BINDAO = BIND_BASE + 2
BINDDO = BIND_BASE + 3
BINDAP = BIND_BASE + 4
BINDDP = BIND_BASE + 5


SP_ADJ = BIND_BASE + 6 
NOOP = 214
NOOP_INIT = 250
CALL = 255 
RET = 254


labelRefs = {}
dataWidths = {'H':2,'h':2,'i':4,'I':4,'f':4,'b':1,'B':1}


instructions = {
    "BINDAI": {'opcode':BINDAI, 'argFormats':['H','B'], 'formatCode':'B'},
    "PUSH_MEM_8": {'opcode':PUSH_MEM_8, 'argFormats':['H'], 'formatCode':'B'},
    "PUSH_CONST_8":{'opcode':PUSH_CONST_8, 'argFormats':['B'], 'formatCode':'B'},
    "POP_REGS_8": {'opcode':POP_REGS_8, 'argFormats':['B'], 'formatCode':'B'},
    "POP_REGS_16": {'opcode':POP_REGS_8 + END_8, 'argFormats':['B'], 'formatCode':'B'},
    "UJMP": {'opcode':UJMP, 'argFormats':['H'], 'formatCode':'B'},
    "CALL": {'opcode':CALL, 'argFormats':['H'], 'formatCode':'B'},
    "MOV_SPREL2_REG_8": {'opcode': MOV_SPREL2_REG_8, 'argFormats':['B','B'], 'formatCode':'B'},
    "RET": {'opcode': RET, 'argFormats':[], 'formatCode':'B'},
    "NOOP": {'opcode': NOOP, 'argFormats':[], 'formatCode':'B'},
    "MOV_REG2_SPREL_8": {'opcode': MOV_REG2_SPREL_8, 'argFormats':['B','B'], 'formatCode':'B'},
    "CMP_INT_8": {'opcode': CMP_INT_8, 'argFormats':['B'], 'formatCode':'B'},
    "MUL_UINT_8": {'opcode': MUL_UINT_8, 'argFormats':['B'], 'formatCode':'B'},
    "JEQ": {'opcode': JEQ, 'argFormats':['H'], 'formatCode':'B'},
    "INC_SPREL_UINT_8": {'opcode': INC_SPREL_UINT_8, 'argFormats':['B'], 'formatCode':'B'},
    "SP_ADJ": {'opcode': SP_ADJ, 'argFormats':['B'], 'formatCode':'B'},

    
}


def chunkifyProgram(filename):
    """ Step 1 in the process is to break the text up into chunks delineated by whitespace """
    commentRe = re.compile("^\s*#")
    
    #print ("Filename: ", filename)
    f = open(filename)
    line = f.readline()
    ip = 0
    program = []
    labelRefs = {}
    while line:
        if not commentRe.match(line):
            opcode = 0
            lineParts = line.split(' ')
            lpStripped = list(map (lambda s: s.strip(),lineParts))
            lpCount = len(lpStripped)
            for chunk in lpStripped:
                if chunk != "":
                    program.append(chunk)
        line = f.readline()
    f.close()
    return program

def stage1(program):    
    """ Look up each chunk and replace with annotated metadata if it's an instruction mnemonic """ 
    i = 0
    for code in program:
        if code in instructions:
            codeData = instructions[code]
            codeData['mnemonic'] = code
            program[i] = codeData.copy()
        #else:
        #    instructions[code] = code
        i = i + 1
    return program


def stage2(program):
    """Reverse the program to turn it into a stack. Pop chunks off the
stack and look at them. If a chunk is an instruction, look at its
metadata and pop the following chunks which represent its arguments
and annotate them as well. If a chunk is neither an instruction or an
argument to one, it is a label."""
    programRev = program
    programRev.reverse()
    outputProgram = []
    while programRev:
        code = programRev.pop()
        if type(code) == dict:
            outputProgram.append(code)
            if 'argFormats' in code:
                for argFormat in code['argFormats']:
                    codearg = programRev.pop()
                    outputProgram.append({'reftext':codearg, 'formatCode':argFormat})            
        else:
            # it's not a recognized instruction or argument so it must
            # be a label.
            outputProgram.append({'reftext':code, 'reftype':'label'})
    return outputProgram


def stage3(program):
    ip = i = 0
    for code in program:
        code['location'] = ip
        program[i] = code
        if 'formatCode' in code:
            width = dataWidths[code['formatCode']]
            #print(str(code) + " :: IP:" + str(ip) + ", width: " + str(width))
            ip = ip + width            
        elif 'reftype' in code and code['reftype'] != 'label':
            #print("IP:" + str(ip) + ", width: 1")
            ip = ip + 1
        i = i + 1
    return program


def stage4(program):
    i = 0
    for code in program:
        if type(code) == dict and 'opcode' in code:
            code = [code['opcode']]
        program[i] = code
        i = i + 1
    return program


def stage5(program):
    #print("PROGRAM is TYPE :" + str(type(program)))
    ip = i = 0
    outputProgram = []
    for code in program:
        if type(code) == dict:
            if 'reftext' in code and 'formatCode' in code:
                outputProgram.append(translator.translate(code['reftext'],code['formatCode']))
        elif type(code) == list:
            outputProgram.append(code)
        i = i + 1
    return list(itertools.chain.from_iterable(outputProgram))

def locateRefs(program):
    for code in program:
        if 'reftext' in code and 'reftype' in code and code['reftype'] == 'label':
            reftext = code['reftext']
            if re.match('[a-zA-Z_]',reftext):
                # ref starts with a letter or underscore
                labelRefs[code['reftext']] = code['location']
            

            



# #print(getNibbles("1,4"))
# print(translator.translate("84",'H'))
# print(translator.translate("84.5",'f'))
# print(translator.translate("84",'b'))
# print(translator.translate("84",'B'))
# print(translator.translate("84",'i'))
# print(translator.translate("84",'I'))
# print(translator.translate("1,4",'B'))



