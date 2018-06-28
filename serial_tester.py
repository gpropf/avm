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
import unittest

baudrate = 57600

### Begin Tests ###
###################

class TestBlink(unittest.TestCase):

    def test_blink(self):
        ser = serial.Serial('/dev/ttyACM1', baudrate, timeout=1)
        responses = sendAndListen(ser)
        print("Sleeping for 2...")
        hexf = open("tests/blink.hex", "r")
        lines = hexf.readlines()

        for line in lines:
            commandSentResponses = sendAndListen(ser, bytes(line,'utf-8'))
            prettyPrintArray(commandSentResponses)

        

        #print("Sleeping for 2...")
        #time.sleep(2)
        commandSentResponses = sendAndListen(ser, b's 500')
        prettyPrintArray(commandSentResponses)
        #time.sleep(2)
        commandSentResponses = sendAndListen(ser,b'r')
        prettyPrintArray(commandSentResponses)
        #time.sleep(2)
        commandSentResponses = sendAndListen(ser,b'q')
        prettyPrintArray(commandSentResponses)
        #time.sleep(2)
        ser.close()

        line43 = commandSentResponses[43].split(":")[1].strip()
        self.assertEqual( int(line43), 255)



    def test_loop_intmath(self):
        ser = serial.Serial('/dev/ttyACM1', baudrate, timeout=1)
        responses = sendAndListen(ser)
        print("Sleeping for 2...")
        hexf = open("tests/loop-intmath.hex", "r")
        lines = hexf.readlines()

        for line in lines:
            commandSentResponses = sendAndListen(ser, bytes(line,'utf-8'))
            prettyPrintArray(commandSentResponses)

        

        #print("Sleeping for 2...")
        #time.sleep(2)
        commandSentResponses = sendAndListen(ser, b's 200')
        prettyPrintArray(commandSentResponses)
        #time.sleep(2)
#        commandSentResponses = sendAndListen(ser,b'r')
#        prettyPrintArray(commandSentResponses)
#        time.sleep(2)
        commandSentResponses = sendAndListen(ser,b'q')
        prettyPrintArray(commandSentResponses)
        #time.sleep(2)
        ser.close()

        testline = commandSentResponses[20].split(":")[1].strip()
        self.assertEqual( int(testline), 25)

        
#################
### End Tests ###        




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
    responses = []
    i = 0
    
    while response:
     #   ser.flush()
        response = ser.readline()
        rdecoded = response.decode()
        responses.append(rdecoded)
        #print(str(i) + ":" + rdecoded, end='')
      #  ser.flush()
        i = i + 1

    print("\n======================================== End Response ===")
    return responses
#    time.sleep(2)

filename = "read-and-blink.hex"

def prettyPrintArray(arr):
    i = 0
    for a in arr:
#        print(str(i) + ":" + a, end = '')
        print(a, end = '')
        i = i + 1

def main():
    ser = serial.Serial('/dev/ttyACM1', baudrate, timeout=1)
    print(ser.name) # check which port was really used

    responses = sendAndListen(ser)

    print("Sleeping for 2...")

    hexf = open(filename, "r")
    lines = hexf.readlines()

    for line in lines:
        commandSentResponses = sendAndListen(ser, bytes(line,'utf-8'))
        prettyPrintArray(commandSentResponses)

        
#    sendAndListen(ser, b'l 0 cf31000fd133000aff1900f9f9f9f9ff2500f9f9')
#    sendAndListen(ser, b'l 20 f9f9ce00003d001bff1000080d083300fe3d001b')
#    sendAndListen(ser, b'l 40 001000080d083300fe31003300')

    print("Sleeping for 2...")
    time.sleep(2)
    commandSentResponses = sendAndListen(ser, b's 500')
    prettyPrintArray(commandSentResponses)
    time.sleep(2)
    commandSentResponses = sendAndListen(ser,b'r')
    prettyPrintArray(commandSentResponses)
    time.sleep(2)
    commandSentResponses = sendAndListen(ser,b'q')
    prettyPrintArray(commandSentResponses)
    time.sleep(2)
    ser.close()

#main()
