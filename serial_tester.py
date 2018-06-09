#!/usr/bin/env python3


# serial_tester.py
# ======================================================================
# Test the VM using the serial API and hot loaded code.

import sys
if sys.version_info[0] != 3 or sys.version_info[1] < 6:
    print("This script requires Python version 3.6 or higher")
    sys.exit(1)

import serial
import io

    

def main():
    ser = serial.Serial('/dev/ttyACM0', 57600)
    print(ser.name) # check which port was really used
    
    ser.write(b'l 100 01020304050607080910\n')

    response = 1
    i = 0
    while response:
        i = i + 1
        print(str(i) + ":After Writing...")
        response = ser.readline()
#        ser.flush()
        print(response.decode())

    print("Writing...")
    ser.write(b'm 80 120\n')
    print("After Writing - - ...")
    response = 1
    i = 0
    while response:
        i = i + 1
        response = ser.readline()
        print(str(i) + ":After Writing...")
        print(response.decode())
   # ser.write(b'm 0 100\n')
   # response = ser.read(10)
    ser.close()
    #print(response)

main()
