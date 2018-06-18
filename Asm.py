import re
import struct, itertools, json
import collections as cl

from instruction_set import *

NO_OPCODE_YET = -100000


labelRefs = {}
#dataWidths = {'H':2,'h':2,'i':4,'I':4,'f':4,'b':1,'B':1, 'N':0.5}
dataBitWidths = {'H':16,'h':16,'i':32,'I':32,'f':32,'b':8,'B':8, 'N':4}

## For argFormats and formatCode below the codes are as follows:
## ====================================================================
## H: 16 bits, typically an address
## B: 8 bits, usually a data constant or stack offset
## N: a "nibble", 4 bits. Used to pack 2 register indices into a single byte.

instructions = {
    "BINDAI": {'opcode':BINDAI, 'argFormats':['H','B'], 'formatCode':'B'},
    "BINDDI": {'opcode':BINDDI, 'argFormats':['H','B'], 'formatCode':'B'},
    "PUSH_MEM_8": {'opcode':PUSH_MEM_8, 'argFormats':['H'], 'formatCode':'B'},
    "PUSH_CONST_8":{'opcode':PUSH_CONST_8, 'argFormats':['V'], 'formatCode':'B'},
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

def leftUnary(leftArg, op):
    """ Process left unary operators. Currently only handles the declaration operator ":". The value of 'op' is ignored."""
    if leftArg['type'] == 'variable' or leftArg['type'] == 'label':
        return {**leftArg, **{'declaration': True}}
    else:
        return leftArg # Really this is an error but I'm not adding the exceptions yet.
        

operators = {':': {'stage':'leftUnary','combinator': leftUnary},
             ',': {'stage':'processTextOperators','combinator':(lambda x,y: y)},
             '.': {'stage':'processTextOperators','combinator':(lambda x,y: float(str(x['text']) + '.' +str(y['text'])))},
             '+': {'stage':'sx','combinator':(lambda x,y: x + y)},
             '-': {'stage':'sx','combinator':(lambda x,y: x - y)},
             '*': {'stage':'sx','combinator':(lambda x,y: x * y)},
             '/': {'stage':'sx','combinator':(lambda x,y: x / y)},
             '//': {'stage':'sx','combinator':(lambda x,y: x // y)},
             '$': {'stage':'processTextOperators','combinator':(lambda x,y: { **y, **{'type':'variable', 'bitWidth': int(x['text']) * 8}})},
             '=': {'stage':'sx','combinator':(lambda x,y: y)}
}

grouping = {'(':'open-parenz',
            ')':'close-parenz',
            '[':'open-square-bracket',
            ']':'close-square-bracket',
            '{':'open-curly-bracket',
            '}':'close-curly-bracket'}

            

def annotateChunk(chunk):
   
    instructionEchelons = {"8":0, "16":1, "32":2}
    #byteWidthIntval = 1
    #chunkBaseName = chunk
   
    lastUnderscoreIndex = chunk.rfind("_")
    if lastUnderscoreIndex != -1: 
        chunkBaseName = chunk[:lastUnderscoreIndex]
        byteWidth = chunk[lastUnderscoreIndex + 1:]

        if byteWidth in instructionEchelons:
            instructionEchelon = instructionEchelons[byteWidth]
            chunk = chunkBaseName + "_8"             
    if chunk in instructions:
        if instructions[chunk]['opcode'] <= END_8:
            chunk = {'text':chunk,
                     'type':'instruction',
                     'instructionEchelon':instructionEchelon,
                     'bitWidth': 8}
        else:
            chunk = {'text':chunk, 'type':'instruction','bitWidth': 8}
    elif chunk in operators:
        chunk = {'text':chunk, 'type':'operator'}
    elif chunk in grouping:
        chunk = {'text':chunk, 'type': grouping[chunk]}
    elif chunk.isdigit():
        chunk = {'text': chunk, 'type':'int', 'value': int(chunk)}
    else:
        chunk = {'text':chunk, 'type':'label', 'bitWidth': 0}
    return chunk


def processTextOperators(program):
    """ Early stage operators processed """
    plen = len(program)
    killNextChunk = False
    for i in range(len(program)):
        if killNextChunk:
            program[i] = ""
            killNextChunk = False
            continue
        chunk = program[i]
        ctype = chunk['type']
        if chunk['type'] == 'operator':
            op = operators[chunk['text']]
#            print(op)
            if op['stage'] == 'processTextOperators':
                combo = op['combinator']
                program[i] = combo(program[i-1],program[i+1])
                program[i-1] = ""
                killNextChunk = True
#                program[i+1] = ""
    return list(filter(lambda x: x != "", program))



def processUnaryOps(program):
    """ Unary operators processed """
    plen = len(program)
    killNextChunk = False
    for i in range(len(program)):
        chunk = program[i]
        ctype = chunk['type']

        if ctype == 'operator':
            op = operators[chunk['text']]
#            print(op)
            if op['stage'] == 'leftUnary':
                combo = op['combinator']
                program[i] = combo(program[i-1],program[i])
                program[i-1] = ""
                
#                program[i+1] = ""
    return list(filter(lambda x: x != "", program))

    
def stripComments(filename, verbose = False):
    if verbose:
        print("================= stripComments =================")
    f = open(filename)
    line = f.readline()
    program = []
    while line:
        line = line.split('#')[0]
        program = program + [line]
        line = f.readline()
    return program
 

def chunkOnDblQuotes(program, verbose = False):
    if verbose:
        print("================= chunkOnQuotes =================")
    outProgram = []
    for line in program:

        chunks = line.split("\"")
        numChunks = len(chunks)
        for i in range(numChunks):
            if i % 2 == 1:
                chunks[i] = {'text':chunks[i], 'type':'DblQuotedChunk'}
        outProgram = outProgram + chunks
    return outProgram

def chunkOnSpaces(program, verbose = False):
    if verbose:
        print("================= chunkOnSpaces =================")
    outProgram = []
    for line in program:
        if type(line) != dict:
            chunks = re.split('([ \n]+)',line)
            chunks = list(filter(lambda c: c.strip() != "", chunks))
            outProgram = outProgram + chunks
        else:
            outProgram = outProgram + [line]

    return outProgram

        
def chunkAndClassify(program, verbose = False):
    
    if verbose:
        print("================= chunkAndClassify =================")
    ip = 0
    labelRefs = {}
    outProgram = []
    for chunk in program:
        
        if type(chunk) == str:
            if re.match("^\d+?\.\d+?$", chunk): # Chunk is a floating point value
                chunk = {'text':chunk, 'value': float(chunk), 'type':'float', 'bitWidth':32}
                # Floats are always going to take up 4 bytes.
                outProgram.append(chunk)
                # No need to classify further, we're done.
                continue
            #opcode = 0
            chunkParts = re.findall(r'\w+|:+|\(|\)|\++|,+|/+|\*+|\-+|\$+', chunk)
            chunkParts = list(map (lambda c: c.strip(),chunkParts))
            chunkParts = list(filter(lambda c: c != "" and c != ",", chunkParts))
            chunkParts = list(map (lambda c: annotateChunk(c),chunkParts))
            #cpCount = len(cpStripped)
            outProgram = outProgram + chunkParts
        else:
            outProgram.append(chunk)
        outProgram = list(filter(lambda c: c != [],outProgram))
    return outProgram


#argFormatBitWidths = { 'H': 16, 'B': 8, 'N': 4, 

def captureIntructionArgs(program, verbose = False):
    bitWidthsByEchelon = [8,16,32]
    if verbose:
        print("================= captureIntructionArgs =================")
    for i in range(len(program)):
        chunk = program[i]
        if chunk['type'] == 'instruction':
            instructionPrototype = instructions[chunk['text']]
            for argFormat in instructionPrototype['argFormats']:
                i = i + 1
                if argFormat == 'V':
                    program[i]['bitWidth'] = bitWidthsByEchelon[chunk['instructionEchelon']]
                else:
                    desiredBitWidth = dataBitWidths[argFormat]
                    if 'bitWidth' in program[i]:
                        existingBitWidth = program[i]['bitWidth']
                        if desiredBitWidth != existingBitWidth:
                            print("WARNING: in " + program[i]['text'])
                            print("WARNING: bit width mismatch, expecting "
                                  + str(desiredBitWidth) + " bits, found " + str(existingBitWidth) + " bits.")

                    program[i]['bitWidth'] = desiredBitWidth
    return program

def buildExpressionText(expList):
    expTexts = []
    for expDict in expList:
        if type(expDict) == dict:
            expTexts.append(expDict['text'])
        else:
            expTexts.append("(")
            expTexts.append(buildExpressionText(expDict))
            expTexts.append(")")
    return (" ").join(expTexts)

def buildExpressionTree(program, r = 0):
    dp = cl.deque(program)
    outList = cl.deque()
    while dp:
        outChunk = dp.popleft()
#        print("r = " + str(r) + ", CHUNK:" + str(outChunk))
        if type(outChunk) == dict:
            #print("CHUNK TEXT:" + outChunk['text'])
            if outChunk['type'] == "open-parenz":
                #print("open-parenz ------------------------------------------")
                (dp, outChunk) = buildExpressionTree(dp, r + 1)
            elif outChunk['type'] == "close-parenz":
                #print("close-parenz ------------------------------------------")
                if r == 1:
                    #print("r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1r=1")
                    outList = {'text':buildExpressionText(outList), 'type':'expression', 'expressionTree': outList}
                return (dp, outList)
            
        outList.append(outChunk)
    return (dp, outList)
            
def indexProgram(program):
    needNibble = False
    byteCount = 0
    for chunk in program:
        chunk['byteIndex'] = byteCount
        bits = 0
        if chunk['type'] == 'instruction':
            bits = 8
        elif 'bitWidth' in chunk:
            bits = chunk['bitWidth']
            if bits == 4:
                if needNibble:
                    bits = 8
                    needNibble = False
                else:
                    needNibble = True
                    bits = 0
        byteCount = byteCount + bits // 8

    return program

def buildSymbolTable(program):
    symbols = {}
    for chunk in program:
        if (chunk['type'] == 'variable' or chunk['type'] == 'label') and 'declaration' in chunk and chunk['declaration'] == True:
            symbols[chunk['text']] = { 'value': chunk['byteIndex'], 'symbolType': chunk['type'], 'bitWidth': chunk['bitWidth']}
    return symbols


def emitByteCodes(program, symbols):
    for chunk in program:
        ctype = chunk['type']
        ctext = chunk['text']
        value = False
        #print("CTYPE: " + ctype)
        if ctype == 'instruction' and ctext in instructions:
            
            value = instructions[ctext]['opcode']
            if opcode <= END_8:
                value = value + END_8 * chunk['instructionEchelon']
            chunk['value'] = value
        elif ctype == 'variable' or ctype == 'label' and ctext in symbols:
            value = symbols[ctext]['value']
            chunk['value'] = value

        return program
            

def tagByteValues(program, symbols):
    for chunk in program:
        ctype = chunk['type']
        ctext = chunk['text']
        value = False
        #print("CTYPE: " + ctype)
        if ctype == 'instruction' and ctext in instructions:
            #print("INSTRUCTION!!!!!!! CTYPE: " + ctype)
            value = instructions[ctext]['opcode']
            if value <= END_8:
                value = value + END_8 * chunk['instructionEchelon']
            chunk['value'] = value
        elif ctype == 'variable' or ctype == 'label' and ctext in symbols:
            value = symbols[ctext]['value']
            chunk['value'] = value
    return program

def emitValues(program):
    formatCodes = { 8: ['b','B'], 16: ['h','H'], 32: ['i','I'] }
    dp = cl.deque(program)
    outp = []
    bytes = []
    while dp:
        bitWidth = 0
        chunk = dp.popleft()
        formatCodeIndex = 1
        if 'bitWidth' in chunk:
            bitWidth = chunk['bitWidth']
        if bitWidth == 0:
            #print("bitwidth == 0!!!!!!!!!!!!!!")
            continue

        if chunk['type'] == 'float':
            bytes = struct.pack('f',chunk['value'])
        else:
            if chunk['type'] == 'sint':
                formatCodeIndex = 0
            if bitWidth == 4:
                lowNibbleChunk = dp.popleft()
                byteVal = chunk['value'] * 16 + lowNibbleChunk['value']
                bytes = struct.pack('B',byteVal)
            else:
                bytes = struct.pack(formatCodes[bitWidth][formatCodeIndex],chunk['value'])
                #print(chunk['text'] + " has bitWidth = " +str(bitWidth) + ", bytes has " + str(len(bytes)) + " bytes")
        outp = outp + [byte for byte in bytes]
        #else:
        #    outp = outp + [chunk['value']]
    return outp
#        print (chunk['value'])

def test():
    p = stripComments("test-math.avm")
    p = chunkOnDblQuotes(p)
    p = chunkOnSpaces(p)
    p = chunkAndClassify(p)
    p = processTextOperators(p)
    p = processUnaryOps(p)
    
    (dp,p) = buildExpressionTree(p)
    p = captureIntructionArgs(p)
    p = indexProgram(p)
    s = buildSymbolTable(p)
    pfinal = tagByteValues(p,s)
    p = emitValues(pfinal)
#    for c in p:
#        print(c)
    return (pfinal,s,p)




                
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

    
def emitInstructionSet(instructions, outf, codeType = "C++"):
    """ Creates the enum and python code for the instruction set from the JSON file."""
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
    """ Creates the C++ header file containing the enum with all the instruction constants."""
    outf = open(filename + ".h", "w")
    with open(filename + ".json", "r") as read_file:
        data = json.load(read_file)
        outf.write ("enum class Opcode : uint8_t {\n")
        outf.write("\n// ----------------------------------- \tVARIABLE DATA WIDTH INSTRUCTIONS \t\n")
        emitInstructionSet(data["instructions"]["multiWidth"], outf)
        outf.write("\n// ----------------------------------- \tFIXED DATA WIDTH INSTRUCTIONS \t\n")
        emitInstructionSet(data["instructions"]["fixedWidth"], outf)
        outf.write ("};\n")
        outf.close()

def buildPythonStub(filename):
    """ Creates the Python file containing the instruction metadata."""
    outf = open(filename + ".py", "w")
    with open(filename + ".json", "r") as read_file:
        data = json.load(read_file)
        outf.write ("###### Python Instruction Codes Constant Block ######\n")
        outf.write("\n## ----------------------------------- \tVARIABLE DATA WIDTH INSTRUCTIONS \t\n")
        mdMultiWidth = emitInstructionSet(data["instructions"]["multiWidth"], outf, "Python")
        outf.write("\n## ----------------------------------- \tFIXED DATA WIDTH INSTRUCTIONS \t\n")
        mdFixedWidth = emitInstructionSet(data["instructions"]["fixedWidth"], outf, "Python")
        outf.write ("### END Python Constants ###\n")
        outf.write ("\n\n### Begin Python Metadata ###\n")
        outf.write("\n## ----------------------------------- \tVARIABLE DATA WIDTH INSTRUCTIONS \t\n")
        outf.write("mdFixedWidth = " + str(mdFixedWidth))
        outf.write("\n## ----------------------------------- \tFIXED DATA WIDTH INSTRUCTIONS \t\n")
        outf.write("mdMultiWidth = " + str(mdMultiWidth))
        outf.write ("\n\n### End Python Metadata ###\n")
        
        outf.close()
        return (mdMultiWidth,mdFixedWidth)
