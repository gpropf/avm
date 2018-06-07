#!/usr/bin/env python3


# make_instruction_set.py
# Script to process a JSON format file representing the VM instruction set and produce various
# things such as Python data structures for the Python assembler and C++ header code
# for the C++ source code.

import json
from Asm import *

def main():
    buildCHeader("instruction_set")
    buildPythonConstants("instruction_set")


main()
