#!/usr/bin/env python3

import argparse, sys

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)




from Asm import *

parser = argparse.ArgumentParser(description='Compile avm files to either C++ header files or REPL hotload commands and hex code of binary.')


parser.add_argument("--cpp", help="Write out C++ header file, default is REPL hotload command blocks",
                    action="store_true")

parser.add_argument("avmfiles", nargs='+')

args = parser.parse_args()


outputSuffix = "hex"
filetype = "hex"

if args.cpp:
    outputSuffix = "h"
    filetype = "C++ header"

for filename in args.avmfiles:
    print("Compiling: " + filename + " to " + filetype + " file...")
    compileAVMFile(filename, outputSuffix)


