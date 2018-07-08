import re, sys
import struct, itertools, json
import collections as cl

from instruction_set import *

NO_OPCODE_YET = -100000

showErrors = False

def eprint(*args, **kwargs):
    if showErrors:
        print(*args, file=sys.stderr, **kwargs)

labelRefs = {}
#dataWidths = {'H':2,'h':2,'i':4,'I':4,'f':4,'b':1,'B':1, 'N':0.5}
dataBitWidths = {'H':16,'h':16,'i':32,'I':32,'f':32,'b':8,'B':8, 'N':4}

## For argFormats and formatCode below the codes are as follows:
## ====================================================================
## H: 16 bits, typically an address
## B: 8 bits, usually a data constant or stack offset
## N: a "nibble", 4 bits. Used to pack 2 register indices into a single byte.

# A note about "operators". I realized that I could get a lot done
# here by making use of the concept of what might be called a
# "compile-time operator". This is something that links tokens
# together or tags them at an early stage of the compilation and which
# is not present at runtime in any sense. In most of CS oeprators work
# at runtime. Something like A = B + C means to assign the *runtime*
# value of the sum of B and C to the variable A.

instructions = { **mdFixedWidth, **mdMultiWidth }

def leftUnary(leftArg, op):
    """ Process left unary operators. Currently only handles the declaration operator ":". The value of 'op' is ignored."""
    if leftArg['type'] == 'variable' or leftArg['type'] == 'label':
        return {**leftArg, **{'declaration': True}} # Return whatever leftArg was but marked up as a 'declaration' IOW.
    else:
        return leftArg # Really this is an error but I'm not adding the exceptions yet.
        
# operators: stage = when is this processed, combinator = function that does the processing of the arguments.
operators = {':': {'stage':'leftUnary','combinator': leftUnary},
             ',': {'stage':'processTextOperators','combinator':(lambda x,y: y)},
             '.': {'stage':'processTextOperators','combinator':(lambda x,y: float(str(x['text']) + '.' +str(y['text'])))},
             '+': {'stage':'sx','combinator':(lambda x,y: x + y)},
             '-': {'stage':'sx','combinator':(lambda x,y: x - y)},
             '*': {'stage':'sx','combinator':(lambda x,y: x * y)},
             '/': {'stage':'sx','combinator':(lambda x,y: x / y)},
             '//': {'stage':'sx','combinator':(lambda x,y: x // y)},

# Dollar ($) is a special operator for variable declarations. '2$FOO:'
# means "create a 2 byte variable called FOO". To use the FOO variable
# you simply write FOO at other places in the code. The location
# assigned to it is determined by where the original declaration
# appears. I mostly put all my globals in a section declared as
# DATASEG: that follows all the code.
             
             '$': {'stage':'processTextOperators','combinator':(lambda x,y: { **y, **{'type':'variable', 'bitWidth': int(x['text']) * 8}})},
             '=': {'stage':'sx','combinator':(lambda x,y: y)}
}

grouping = {'(':'open-parenz',
            ')':'close-parenz',
            '[':'open-square-bracket',
            ']':'close-square-bracket',
            '{':'open-curly-bracket',
            '}':'close-curly-bracket'}


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
        print("================= chunkOnDblQuotes =================")
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
    """ Fine grained chunking of text and early identification of things
# like floating point numbers and integers."""
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


def captureIntructionArgs(program, verbose = False):
    """Looks up each instruction and what kind of args it takes. Tags the
objects that follow accordingly so as to create instruction/argument
groups."""
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
                            eprint("WARNING: in " + str(chunk))
                            eprint("WARNING: in " + program[i]['text'])
                            eprint("WARNING: bit width mismatch, expecting "
                                  + str(desiredBitWidth) + " bits, found " + str(existingBitWidth) + " bits.")

                    program[i]['bitWidth'] = desiredBitWidth
    return program


def buildExpressionText(expList):
    """ Builds a text representation of the expression from the syntax tree. Probably will always remain experimental as I plan on moving complex compile-time shenanigans to the high level language side."""
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
    """Somewhat experimental still. Meant to process compile-time math
 expressions like (V1 + 5) where V1 is the address of a variable."""
    dp = cl.deque(program)
    outList = cl.deque()
    while dp:
        outChunk = dp.popleft()
        if type(outChunk) == dict:
            if outChunk['type'] == "open-parenz":
                (dp, outChunk) = buildExpressionTree(dp, r + 1)
            elif outChunk['type'] == "close-parenz":
                if r == 1:
                    outList = {'text':buildExpressionText(outList),
                               'type':'expression',
                               'expressionTree': outList}
                return (dp, outList)            
        outList.append(outChunk)
    return (dp, outList)


def identifyContextBlocks(program):
    dp = cl.deque(program)
    outList = cl.deque()
    while dp:
        context = None
        outChunk = dp.popleft()
        if outChunk['text'] == "BEGIN":
            context = 'beginContext'
        elif outChunk['text'] == "END":
            context = 'endContext'
        outList.append(outChunk)
        if context:
            outChunk = dp.popleft()
            outChunk['context'] = context
            outList.append(outChunk)
    return outList
            

def indexProgram(program):
    """We should now have enough information to note the byte index of
each object in the program. We take account of the fact there are 2
nibbles to a byte here as well."""
    
    needNibble = False
    byteCount = 0
    context = None
    for chunk in program:
        if context:
            pass
#            print(context + " : " + str(byteCount) + " : " + str(chunk))
        else:
            pass
#            print(str(byteCount) + " : " + str(chunk))
        chunk['byteIndex'] = byteCount
        bits = 0
        if chunk['type'] == 'instruction':
            bits = 8
            
        #elif 'resetIndex' in chunk:
        #    oldByteCount = byteCount
        #    resetIndex = chunk['resetIndex']
            #print("Found resetIndex " + str(resetIndex) + " at old byte count " + str(oldByteCount))
        elif 'context' in chunk:
            ctx = chunk['context']
            if ctx == 'beginContext':
                context = chunk['text']
            elif ctx == 'endContext':
                context = None
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
    """ Builds the symbol table so that variables and labels can be resolved to numeric values."""
    symbols = {}
    for chunk in program:
        if (chunk['type'] == 'variable' or chunk['type'] == 'label') and 'declaration' in chunk and chunk['declaration'] == True:
            chunkVal = chunk['byteIndex']
            symbols[chunk['text']] = { 'value': chunkVal, 'symbolType': chunk['type'], 'bitWidth': chunk['bitWidth']}
    return symbols
         

def tagByteValues(program, symbols):
    """Lookup symbol values in instruction list and user-defined symbol
table, add them to the chunks."""
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
    """The last stage here. Everything should have been resolved to a
value and now we write out an array of numbers representing the
bytecode."""
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
        outp = outp + [byte for byte in bytes]
    return outp



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
    

def makeMetadataForOpcode(mnemonic, argFormats):
    """Tries to resolve the mnemonic string as code.  Failing that,
 assigns a placeholder value until we can add the opcode to the list
 of opcodes."""
    try:
        opcode = eval(mnemonic)
    except:
        opcode = NO_OPCODE_YET
        
    md = {'opcode': opcode, 'argFormats':argFormats }
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
        if "argFormats" in instr and codeType != "C++":
            argFormats = instr["argFormats"]
            metadata[mnemonic] = makeMetadataForOpcode(mnemonic, argFormats)
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
        outf.write("\n## ----------------------------------- \tFIXED DATA WIDTH INSTRUCTIONS \t\n")
        outf.write("mdFixedWidth = " + str(mdFixedWidth))
        outf.write("\n## ----------------------------------- \tVARIABLE DATA WIDTH INSTRUCTIONS \t\n")
        outf.write("mdMultiWidth = " + str(mdMultiWidth))
        outf.write ("\n\n### End Python Metadata ###\n")
        
        outf.close()
        return (mdMultiWidth,mdFixedWidth)

def printChunks(p):
    if type(p) == dict:
        for k,v in p.items():
            print(str(k) + ":" + str(v))
    else:
        for chunk in p:
            print(chunk)
    
def test():
    """ Quickie test script meant to be run by just doing up-arrow at the ipython cmd line."""
    p = stripComments("tests/loop-intmath.avm")
    p = chunkOnDblQuotes(p)
    p = chunkOnSpaces(p)
    
    print("chunkOnSpaces ==================================")
    printChunks(p)
    

    print("chunkAndClassify ==================================")
    p = chunkAndClassify(p)
    printChunks(p)

    print("processTextOperators ==================================")
    p = processTextOperators(p)
    printChunks(p)

    print("processUnaryOps ==================================")
    p = processUnaryOps(p)
    printChunks(p)
    
    print("buildExpressionTree ==================================")
    (dp,p) = buildExpressionTree(p)
    printChunks(p)

    print("identifyContextBlocks ==================================")
    p = identifyContextBlocks(p)
    printChunks(p)
    
    print("captureIntructionArgs ==================================")
    p = captureIntructionArgs(p)
    printChunks(p)

    print("indexProgram ==================================")
    p = indexProgram(p)
    printChunks(p)

    print("buildSymbolTable SYMBOL TABLE: ==================================")
    s = buildSymbolTable(p)
    printChunks(s)

    print("tagByteValues ==================================")
    pfinal = tagByteValues(p,s)
    printChunks(p)

    print("emitValues ==================================")
    p = emitValues(pfinal)
    printChunks(p)

#    for c in p:
#        print(c)
    return (pfinal,s,p)

