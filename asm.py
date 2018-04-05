#!/usr/bin/env python3

import sys

from Asm import *

filename = sys.argv[1]

program = chunkifyProgram(filename)


p1 = stage1(program)
print ("Program After Stage 1:\n=========================\n")
for code in p1:
    print(code)

print ("\n\nProgram After Stage 2:\n=========================\n")
p2 = stage2(p1)
for code in p2:
    print(code)

print ("\n\nProgram After Stage 3:\n=========================\n")
p3 = stage3(p2)
#print("P3 is TYPE:" + str(type(p3)))
for code in p3:
#    print(str(type(code)))
    print(code)

    

print ("\n\nReferences :\n=========================\n")
locateRefs(p3)
    
print(labelRefs)


print ("\n\nProgram After Stage 4:\n=========================\n")
p4 = stage4(p3)
for code in p4:
#    print(str(type(code)))
    print(code)


print ("\n\nProgram After Stage 5:\n=========================\n")
p5 = stage5(p4)
for code in p5:
#    print(str(type(code)))
    print(code)
