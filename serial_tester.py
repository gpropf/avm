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
    

def main():
    ser = serial.Serial('/dev/ttyACM0', baudrate, timeout=1)
    print(ser.name) # check which port was really used
#    ser.flush()
 
    

    response = 1
    i = 0
    while response:
        i = i + 1
        #print(str(i) + ":After Writing...")
        response = ser.readline()
        print(str(i) + ":After Init:" + response.decode(), end='')
 #       ser.flush()

    print("Sleeping for 2...")
    time.sleep(2)
    print("Loading data...")
    ser.write(b'l 0 cf31000fd133000aff1900f9f9f9f9ff2500f9f9\n')
 
    print("============================== After Writing =========================")
    response = 1
    i = 0
    while response:
        i = i + 1
        response = ser.readline()
        print(str(i) + ":" + response.decode(), end='')
    print("Sleeping for 2...")
    time.sleep(2)
    
    ser.write(b'l 20 f9f9ce00003d001bff1000080d083300fe3d001b\n')
    print("============================== After Writing =========================")
    response = 1
    i = 0
    while response:
        i = i + 1
        response = ser.readline()
        print(str(i) + ":" + response.decode(), end='')
    print("Sleeping for 2...")
    time.sleep(2)

    ser.write(b'l 40 001000080d083300fe31003300\n')
    print("============================== After Writing =========================")
    response = 1
    i = 0
    while response:
        i = i + 1
        response = ser.readline()
        print(str(i) + ":" + response.decode(), end='')
    print("Sleeping for 2...")
    time.sleep(2)
    
    

    ser.write(b's 500\n')
    time.sleep(1)
    print("============================== After Writing =========================")
    response = 1
    i = 0
    while response:
        i = i + 1
        response = ser.readline()
        print(str(i) + ":" + response.decode(), end='')


    ser.write(b'r\n')
    time.sleep(1)
    print("============================== After Writing =========================")
    response = 1
    i = 0
    while response:
        i = i + 1
        response = ser.readline()
        print(str(i) + ":" + response.decode(), end='')


    print("Reading Memory...")

    ser.write(b'q\n')
#    ser.write(b'm 0 40\n')
    time.sleep(1)
    print("============================== After Writing =========================")
    response = 1
    i = 0
    while response:
        i = i + 1
        response = ser.readline()
        print(str(i) + ":" + response.decode(), end='')
        #        print(response.decode())
   # ser.write(b'm 0 100\n')
   # response = ser.read(10)
    ser.close()
    #print(response)

main()
