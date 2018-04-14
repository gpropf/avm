#!/usr/bin/env python3

import sys

from Asm import *

filename = sys.argv[1]

verbosity = True
verbosity = False

program = chunkifyProgram(filename, verbose = verbosity)
#print ("Program After Chunking:\n=========================\n")
#for code in program:
#    print(code)



p1 = stage1(program, verbose = verbosity)

#print ("Program After Stage 1:\n=========================\n")
#for code in p1:
#    print(code)



p2 = stage2(p1, verbose = verbosity)
p3 = stage3(p2, verbose = verbosity)
locateRefs(p3, verbose = verbosity)    
p4 = stage4(p3, verbose = verbosity)
p5 = stage5(p4, verbose = verbosity)

def printStages():

   

    

    print ("\n\nProgram After Stage 2:\n=========================\n")

    for code in p2:
        print(code)
            
    print ("\n\nProgram After Stage 3:\n=========================\n")
    
    #print("P3 is TYPE:" + str(type(p3)))
    for code in p3:
        #    print(str(type(code)))
        print(code)
        
    print ("\n\nReferences :\n=========================\n")

    print(labelRefs)
    print ("\n\nProgram After Stage 4:\n=========================\n")
    
    for code in p4:
        #    print(str(type(code)))
        print(code)
        
    print ("\n\nProgram After Stage 5:\n=========================\n")

    for code in p5:
        #    print(str(type(code)))
        print(code)


#print ("\n\nProgram As C-style array:\n=========================\n")

cstr = ""
i = 0
for code in p5:
    i = i + 1
    cstr = cstr + str(code) + ","
    if i % 10 == 0:
        cstr = cstr + "\n"

#cFormattedString = ','.join(map(lambda n: str(n), p5))
#    print(str(type(code)))

#printStages()
#print(cFormattedString)
print(cstr)
