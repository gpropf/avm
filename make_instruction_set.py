#!/usr/bin/env python3


# make_instruction_set.py
# Script to process a JSON format file representing the VM instruction set and produce various
# things such as Python data structures for the Python assembler and C++ header code
# for the C++ source code.

import json

def emitCode(instructions, codeType = "C++"):
    currentBase = ""
    offset = 0
    for instr in instructions:
        base = instr["base"]
        if base != currentBase:
            currentBase = base
            offset = 0
        else:
            offset = offset + 1
        print (instr["mnemonic"] + " = " + base + " + " + str(offset) + ",")
    


with open("instruction_set.json", "r") as read_file:
    data = json.load(read_file)
    emitCode(data["instructions"]["multiWidth"])
    print()
    emitCode(data["instructions"]["fixedWidth"])

