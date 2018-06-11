

import re

import struct, itertools, json

from instruction_set import *

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


def getNibblesFromInts(nibblePair):
    (na,nb) = nibblePair
    return [na * 16 + nb]
    
def getFloat(text,formatCode):
    bytes = struct.pack(formatCode,float(text))
    return [byte for byte in bytes]

def getInt(text, formatCode):
    bytes = struct.pack(formatCode,int(text))
    return [byte for byte in bytes]
    

#pnibbles = re.compile("[0-9]+,[0-9]+")
pfloat = re.compile("[0-9]+\.[0-9]*")
pint = re.compile("[0-9]+")

ph = { pfloat:getFloat, pint:getInt}



translator = RefTranslator(ph)

"""

MATH_BASE_8 = 0
ADD_UINT_8 = MATH_BASE_8
SUB_UINT_8 = MATH_BASE_8 + 1
MUL_UINT_8 = MATH_BASE_8 + 2
DIV_UINT_8 = MATH_BASE_8 + 3
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
PRINT_AS = BIND_BASE + 7
NOOP = 249
NOOP_INIT = 250
CALL = 255 
RET = 254

"""

NO_OPCODE_YET = -100000


labelRefs = {}
dataWidths = {'H':2,'h':2,'i':4,'I':4,'f':4,'b':1,'B':1, 'N':'N'}

## For argFormats and formatCode below the codes are as follows:
## ====================================================================
## H: 16 bits, typically an address
## B: 8 bits, usually a data constant or stack offset
## N: a "nibble", 4 bits. Used to pack 2 register indices into a single byte.

instructions = {
    "BINDAI": {'opcode':BINDAI, 'argFormats':['H','B'], 'formatCode':'B'},
    "BINDDI": {'opcode':BINDDI, 'argFormats':['H','B'], 'formatCode':'B'},
    "PUSH_MEM_8": {'opcode':PUSH_MEM_8, 'argFormats':['H'], 'formatCode':'B'},
    "PUSH_CONST_8":{'opcode':PUSH_CONST_8, 'argFormats':['B'], 'formatCode':'B'},
    "POP_REGS_8": {'opcode':POP_REGS_8, 'argFormats':['N','N'], 'formatCode':'B'},
    "PRINT_AS": {'opcode':PRINT_AS, 'argFormats':['N','N'], 'formatCode':'B'},
    "POP_REGS_16": {'opcode':POP_REGS_8 + END_8, 'argFormats':['N','N'], 'formatCode':'B'},
    "UJMP": {'opcode':UJMP, 'argFormats':['H'], 'formatCode':'B'},
    "CALL": {'opcode':CALL, 'argFormats':['H'], 'formatCode':'B'},
    "MOV_SPREL2_REG_8": {'opcode': MOV_SPREL2_REG_8, 'argFormats':['B','B'], 'formatCode':'B'},
    "MOV_MEM2_REG_16": {'opcode': MOV_MEM2_REG_8 + END_8, 'argFormats':['H','B'], 'formatCode':'B'},  
    "RET": {'opcode': RET, 'argFormats':[], 'formatCode':'B'},
    "NOOP": {'opcode': NOOP, 'argFormats':[], 'formatCode':'B'},
    "MOV_REG2_SPREL_8": {'opcode': MOV_REG2_SPREL_8, 'argFormats':['B','B'], 'formatCode':'B'},
    "CMP_INT_8": {'opcode': CMP_INT_8, 'argFormats':['N','N'], 'formatCode':'B'},
    "MUL_UINT_8": {'opcode': MUL_UINT_8, 'argFormats':['N','N'], 'formatCode':'B'},
    "SUB_UINT_8": {'opcode': SUB_UINT_8, 'argFormats':['N','N'], 'formatCode':'B'},
    "JEQ": {'opcode': JEQ, 'argFormats':['H'], 'formatCode':'B'},
    "INC_SPREL_UINT_8": {'opcode': INC_SPREL_UINT_8, 'argFormats':['B'], 'formatCode':'B'},
    "SP_ADJ": {'opcode': SP_ADJ, 'argFormats':['B'], 'formatCode':'B'},
}

instructions = { **instructions, **mdFixedWidth, **mdMultiWidth }


def chunkifyProgram(filename, verbose = False):
    """ Step 1 in the process is to break the text up into chunks delineated by whitespace """
    commentRe = re.compile("^\s*#")
    if verbose:
        print("================= chunkifyProgram =================")
    #print ("Filename: ", filename)
    f = open(filename)
    line = f.readline()
    ip = 0
    program = []
    labelRefs = {}
    while line:
        line = line.split('#')[0]
        opcode = 0
        lineParts = line.split(' ')
        lpStripped = list(map (lambda s: s.strip(),lineParts))
        lpStripped = list(filter(lambda x: x != "", lpStripped))
        lpCount = len(lpStripped)
        for chunk in lpStripped:
            commaDelimitedChunks = chunk.split(',')
            commaDelimitedChunks = list(filter(lambda x: x != "", commaDelimitedChunks))
            for cdc in commaDelimitedChunks:
                program.append(cdc)
                if verbose:
                    print (cdc)
        line = f.readline()
    f.close()
    return program


def stage1(program, verbose = False):    
    """ Look up each chunk and replace with annotated metadata if it's an instruction mnemonic """ 
    i = 0
    if verbose:
        print("================= stage1 =================")
    for code in program:
        if code in instructions:
                
            codeData = instructions[code]
            
            byteWidth = code[code.rfind("_")+1:]
            if byteWidth in ["8","16","32"]:
                byteWidth = int(byteWidth) // 8
                codeData['byteWidth'] = byteWidth
            codeData['mnemonic'] = code
            program[i] = codeData.copy()
        #else:
        if verbose:
            print (program[i])
        #    instructions[code] = code
        i = i + 1
    return program


def stage2(program, verbose = False):
    """Reverse the program to turn it into a stack. Pop chunks off the
stack and look at them. If a chunk is an instruction, look at its
metadata and pop the following chunks which represent its arguments
and annotate them as well. If a chunk is neither an instruction or an
argument to one, it is a label."""
    if verbose:
        print("================= stage2 =================")
    programRev = program
    programRev.reverse()
    outputProgram = []
    while programRev:
        code = programRev.pop()
        if type(code) == dict:
            outputProgram.append(code)
            if verbose:
                print(code)
            if 'argFormats' in code:
                for argFormat in code['argFormats']:
                    codearg = programRev.pop()
                    codeHash = {'reftext':codearg, 'formatCode':argFormat}
                    outputProgram.append(codeHash)
                    if verbose:
                        print(codeHash)
        else:
            # it's not a recognized instruction or argument so it must
            # be a label.            
            codeHash = {'reftext':code, 'reftype':'label'}
            outputProgram.append(codeHash)
            if verbose:
                print(codeHash)

    return outputProgram


def stage3(program, verbose = False):

    if verbose:
        print("================= stage3 =================")
    ip = i = 0
    nibbleFound = False
    for code in program:

        code['location'] = ip
        program[i] = code
        if 'formatCode' in code:
            width = dataWidths[code['formatCode']]
            if width == 'N':                
                if nibbleFound:
                    width = 1
                    nibbleFound = False
                else:
                    width = 0
                    nibbleFound = True
                    
            #print(str(code) + " :: IP:" + str(ip) + ", width: " + str(width))
            ip = ip + width            
        elif 'reftype' in code and code['reftype'] != 'label':
            #print("IP:" + str(ip) + ", width: 1")
            ip = ip + 1
        if verbose:
            print(code)
        i = i + 1
    return program


def stage4(program, verbose = False):
    if verbose:
        print("================= stage4 =================")
    i = 0
    for code in program:
        if type(code) == dict and 'opcode' in code:
            code = [code['opcode']]
            if 'byteWidth' in code:
                code = code + END_8 * (byteWidth - 1)

        program[i] = code
        if verbose:
            print(code)
        i = i + 1
    return program


def stage5(program, verbose = False):
    #print("PROGRAM is TYPE :" + str(type(program)))
    if verbose:
        print("================= stage5 =================")
    ip = i = 0
    outputProgram = []
    currentNibbles = []
    for code in program:
        if type(code) == dict:
            if 'reftext' in code and 'formatCode' in code:
                if code['formatCode'] == 'N':
                    nibbleVal = int(code['reftext'])
                    currentNibbles.append(nibbleVal)
                    if verbose:
                        print("NIBBLES!!!: " + str(currentNibbles))
                    if len(currentNibbles) > 1:
                        byteVal = getNibblesFromInts(currentNibbles)
                        outputProgram.append(byteVal)
                        if verbose:
                            print("BYTE VAL COMPLETE ~~~ : " + str(byteVal))
                        currentNibbles = []
                else:
                    codeToAppend = translator.translate(code['reftext'],code['formatCode'])
                    outputProgram.append(codeToAppend)
                    if verbose:
                        print(codeToAppend)
        elif type(code) == list:
            if verbose:
                print(code)
            outputProgram.append(code)
        i = i + 1
    return list(itertools.chain.from_iterable(outputProgram))


def locateRefs(program, verbose = False):
    if verbose:
        print("================= locateRefs =================")
    for code in program:
        if 'reftext' in code and 'reftype' in code and code['reftype'] == 'label':
            reftext = code['reftext']
            if re.match('[a-zA-Z_]',reftext):
                # ref starts with a letter or underscore
                labelRefs[code['reftext']] = code['location']

                
def printAsCStr(program):
    """print program As C-style uint8_t array:"""
    cstr = ""
    i = 0
    for code in program:
        i = i + 1
        cstr = cstr + str(code) + ","
        if i % 10 == 0:
            cstr = cstr + "\n"

    print(cstr)
  

def printAsHexString(program, addr = 0):
    """print program as block of hex characters:"""
    commandPrefix = "l "
    hexstr = ""
    i = 0
    for code in program:
        if i % 20 == 0:
            hexstr = hexstr + "\n" + commandPrefix + str(addr) + " "
        hexDigits = hex(code)[2:]
        if len(hexDigits) == 1:
            hexDigits = "0" + hexDigits
        hexstr = hexstr + hexDigits
        addr = addr + 1
        i = i + 1
        #print(hexstr)
    print(hexstr)
    

def makeMetadataForOpcode(mnemonic, argFormats, formatCode):
    try:
        opcode = eval(mnemonic)
    except:
        opcode = NO_OPCODE_YET
        
    md = {'opcode': opcode, 'argFormats':argFormats, 'formatCode':formatCode}
    return md

    
def emitCode(instructions, outf, codeType = "C++"):
    currentBase = ""
    singleLineCommentStart = "// "
    offset = 0
    comma = ", "
    metadata = {}
    if codeType != "C++":
        singleLineCommentStart = "## "
        comma = " "

    for instr in instructions:
        mnemonic = instr["mnemonic"]
        comments = ""
        if "argFormats" in instr and "formatCode" in instr and codeType != "C++":
            argFormats = instr["argFormats"]
            formatCode = instr["formatCode"]
            metadata[mnemonic] = makeMetadataForOpcode(mnemonic, argFormats, formatCode)
        if "comments" in instr:
            comments = singleLineCommentStart + instr["comments"]
        base = instr["base"]
        if base != currentBase:
            currentBase = base
            offset = 0
        else:
            offset = offset + 1
        outf.write (instr["mnemonic"] + " = " + base + " + " + str(offset) + comma + comments + "\n")
    return metadata
    


def buildCHeader(filename):
    outf = open(filename + ".h", "w")
    with open(filename + ".json", "r") as read_file:
        data = json.load(read_file)
        outf.write ("enum class Opcode : uint8_t {\n")
        outf.write("\n// ----------------------------------- \tVARIABLE DATA WIDTH INSTRUCTIONS \t\n")
        emitCode(data["instructions"]["multiWidth"], outf)
        outf.write("\n// ----------------------------------- \tFIXED DATA WIDTH INSTRUCTIONS \t\n")
        emitCode(data["instructions"]["fixedWidth"], outf)
        outf.write ("};\n")
        outf.close()

def buildPythonConstants(filename):
    outf = open(filename + ".py", "w")
    with open(filename + ".json", "r") as read_file:
        data = json.load(read_file)
        outf.write ("###### Python Instruction Codes Constant Block ######\n")
        outf.write("\n## ----------------------------------- \tVARIABLE DATA WIDTH INSTRUCTIONS \t\n")
        mdMultiWidth = emitCode(data["instructions"]["multiWidth"], outf, "Python")
        outf.write("\n## ----------------------------------- \tFIXED DATA WIDTH INSTRUCTIONS \t\n")
        mdFixedWidth = emitCode(data["instructions"]["fixedWidth"], outf, "Python")
        outf.write ("### END Python Constants ###\n")
        outf.write ("\n\n### Begin Python Metadata ###\n")
        outf.write("\n## ----------------------------------- \tVARIABLE DATA WIDTH INSTRUCTIONS \t\n")
        outf.write("mdFixedWidth = " + str(mdFixedWidth))
        outf.write("\n## ----------------------------------- \tFIXED DATA WIDTH INSTRUCTIONS \t\n")
        outf.write("mdMultiWidth = " + str(mdMultiWidth))
        outf.write ("\n\n### End Python Metadata ###\n")
        
        outf.close()
        return (mdMultiWidth,mdFixedWidth)
