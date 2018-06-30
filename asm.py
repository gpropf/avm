#!/usr/bin/env python3

import sys

from Asm import *

filename = sys.argv[1]

verbosity = True
#verbosity = False

#program = chunkifyProgram(filename, verbose = verbosity)
#p1 = stage1(program, verbose = verbosity)
#p2 = stage2(p1, verbose = verbosity)
#p3 = stage3(p2, verbose = verbosity)
#locateRefs(p3, verbose = verbosity)    
#p4 = stage4(p3, verbose = verbosity)
#p5 = stage5(p4, verbose = verbosity)

p = stripComments(filename)
p = chunkOnDblQuotes(p)
p = chunkOnSpaces(p)
p = chunkAndClassify(p)
p = processTextOperators(p)
p = processUnaryOps(p)

(dp,p) = buildExpressionTree(p)
p = captureIntructionArgs(p)
p = indexProgram(p)
s = buildSymbolTable(p)
p = tagByteValues(p,s)
p = emitValues(p)

outputHex = True
if len(sys.argv) > 2:
    arg1 = sys.argv[2]
    if arg1 == "--C++":
        outputHex = False

        

if outputHex:
    printAsHexString(p)
else:
    printAsCStr(p)
print()

