#!/usr/bin/env python3


# serial_tester.py
# ======================================================================
# Test the VM using the serial API and hot loaded code.
# Usage: 'py.test serial_tester.py' or 'python -m pytest serial_tester.py'

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

def getSerialDevice():
    serialDevices = ["/dev/ttyACM0", "/dev/ttyACM1"]
    # a list of serial devices to try to open before each test. Necessary
    # because the device changes a lot as the system remaps the Arduino
    # serial port
    sdev = None
    for devName in serialDevices:
        try:
            sdev = serial.Serial(devName, baudrate, timeout=1)
            break
        except:
            print ("Failed to open serial device: " + devName + ". Trying next device...")
    return sdev


class TestAVM(unittest.TestCase):

    

    def test_blink(self):
        #ser = serial.Serial('/dev/ttyACM0', baudrate, timeout=1)
        ser = getSerialDevice()
        responses = sendAndListen(ser)
        hexf = open("tests/blink.hex", "r")
        lines = hexf.readlines()

        for line in lines:
            commandSentResponses = sendAndListen(ser, bytes(line,'utf-8'))
            prettyPrintArray(commandSentResponses)
            
        hexf.close()
        commandSentResponses = sendAndListen(ser, b's 500')
        prettyPrintArray(commandSentResponses)
        commandSentResponses = sendAndListen(ser,b'r')
        prettyPrintArray(commandSentResponses)
        commandSentResponses = sendAndListen(ser,b'q')
        prettyPrintArray(commandSentResponses)
        ser.close()
        testline = commandSentResponses[44].split(":")[1].strip()
        self.assertEqual( int(testline), 255)
        


    def test_loop_intmath(self):
        ser = getSerialDevice()
        time.sleep(2)
        responses = sendAndListen(ser)
        hexf = open("tests/loop-intmath.hex", "r")
        lines = hexf.readlines()

        for line in lines:
            commandSentResponses = sendAndListen(ser, bytes(line,'utf-8'))
            prettyPrintArray(commandSentResponses)

        hexf.close()
        commandSentResponses = sendAndListen(ser, b's 200')
        prettyPrintArray(commandSentResponses)
        commandSentResponses = sendAndListen(ser,b'q')
        prettyPrintArray(commandSentResponses)
        ser.close()
        testline = commandSentResponses[21].split(":")[1].strip()
        self.assertEqual( int(testline), 25)

        
    def test_floatmath(self):
        ser = getSerialDevice()
#        serial.Serial('/dev/ttyACM0', baudrate, timeout=1)
        responses = sendAndListen(ser)
        hexf = open("tests/floatmath.hex", "r")
        lines = hexf.readlines()

        for line in lines:
            commandSentResponses = sendAndListen(ser, bytes(line,'utf-8'))
            prettyPrintArray(commandSentResponses)

        hexf.close()
        commandSentResponses = sendAndListen(ser, b's 20')
        prettyPrintArray(commandSentResponses)
        ser.close()
        testline = commandSentResponses[1].strip()
        self.assertEqual( float(testline), 407.02)
        

        
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
    time.sleep(1)

    responses = []
    stringRepeats = 5
    blockMarker = "=*="*stringRepeats
    responses.append("\n" + blockMarker + " Begin Response " + blockMarker + "\n")
    response = 1
    i = 0
    
    while response:
        response = ser.readline()
        try:
            rdecoded = response.decode()
            responses.append(rdecoded)
        except:
            print("Error during decoding, trying to continue...")
            print("Begin Failed decoding:---------")
            print(response)
            print("End Failed decoding:---------")
            
        i = i + 1

    responses.append("\n" + blockMarker + " End Response " + blockMarker + "\n")
    return responses


#filename = "tests/floatmath.hex"

def prettyPrintArray(arr, showLineNumbers = False):
    if showLineNumbers:
        i = 0
        for a in arr:
            print(str(i) + ":" + a, end = '')
            i = i + 1
    else:
        for a in arr:
            print(a, end = '')


            
if __name__ == '__main__':
    unittest.main()
