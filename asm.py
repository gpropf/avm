#!/usr/bin/env python3

import sys

from Asm import *

filename = sys.argv[1]

verbosity = True
#verbosity = False

program = chunker(filename, verbose = verbosity)

#processPseudoCode(program)
print("========================")
#print(pseudoCodeRefs)

# p1 = stage1(program, verbose = verbosity)
# p2 = stage2(p1, verbose = verbosity)
# p3 = stage3(p2, verbose = verbosity)
# locateRefs(p3, verbose = verbosity)    
# p4 = stage4(p3, verbose = verbosity)
# p5 = stage5(p4, verbose = verbosity)

# printAsCStr(p5)
