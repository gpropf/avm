#!/usr/bin/env bash


echo
echo '###################################################################'
echo -------------------- AVM Test Suite Script ------------------------
echo '###################################################################'
echo "To make sure serial_tester.py works make sure the AVM is running"
echo "in 'quiet' mode with VERBOSE option turned on in config.h"
echo '###################################################################'
echo

echo "Building binaries for testing..."
./asm.py tests/*.avm
echo

echo "Runnning serial_tester.py script..."
py.test serial_tester.py

