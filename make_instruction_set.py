#!/usr/bin/env python3


# make_instruction_set.py
# Script to process a JSON format file representing the VM instruction set and produce various
# things such as Python data structures for the Python assembler and C++ header code
# for the C++ source code.


import sys
if sys.version_info[0] != 3 or sys.version_info[1] < 6:
    print("This script requires Python version 3.6 or higher")
    sys.exit(1)

import json
from Asm import *

#(mdMultiWidth, mdFixedWidth) = (0,0)

def main():
    buildCHeader("instruction_set")
    (mdMultiWidth, mdFixedWidth) = buildPythonConstants("instruction_set")
    #return(mdMultiWidth,mdFixedWidth)

#(mdMultiWidth, mdFixedWidth) = main()
main()
