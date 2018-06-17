#!/usr/bin/env python3


# serial_tester.py
# ======================================================================
# Test the VM using the serial API and hot loaded code.

import sys, time
if sys.version_info[0] != 3 or sys.version_info[1] < 6:
    print("This script requires Python version 3.6 or higher")
    sys.exit(1)

import serial
import io

baudrate = 57600

def sendAndListen(ser, data = None, terminalNewline = b'\n', echoSentData = True):
    print("Writing data...")
    
    if data:
        if echoSentData:
            print(data + terminalNewline)
        ser.flush()
        ser.write(data + terminalNewline)
        ser.flush()
    else:
        ser.flush()
        ser.write(terminalNewline)
        ser.flush()
    #ser.flush()
    time.sleep(1)
    #ser.flush()
    print("========================================== Begin Response ===")
    response = 1
    i = 0
    while response:
     #   ser.flush()
        response = ser.readline()
        print(str(i) + ":" + response.decode(), end='')
      #  ser.flush()
        i = i + 1

    print("\n======================================== End Response ===")
    time.sleep(2)

filename = "read-and-blink.hex"

def main():
    ser = serial.Serial('/dev/ttyACM0', baudrate, timeout=1)
    print(ser.name) # check which port was really used

    sendAndListen(ser)

    print("Sleeping for 2...")

    hexf = open(filename, "r")
    lines = hexf.readlines()

    for line in lines:
        sendAndListen(ser, bytes(line,'utf-8'))

        
#    sendAndListen(ser, b'l 0 cf31000fd133000aff1900f9f9f9f9ff2500f9f9')
#    sendAndListen(ser, b'l 20 f9f9ce00003d001bff1000080d083300fe3d001b')
#    sendAndListen(ser, b'l 40 001000080d083300fe31003300')

    print("Sleeping for 2...")
    time.sleep(2)
    sendAndListen(ser, b's 500')
    time.sleep(2)
    sendAndListen(ser,b'r')
    time.sleep(2)
    sendAndListen(ser,b'q')
    time.sleep(2)
    ser.close()

main()
