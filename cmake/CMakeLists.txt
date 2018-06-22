cmake_minimum_required(VERSION 3.5.1)
project(avmx86)

#There are lots of scripts with cmake
#for finding external libraries. 
#see /usr/local/share/cmake-2.6/Modules/Find*.cmake for more examples
#find_package(GLUT)
#find_package(OpenGL)

set(ARDUNIO_INSTALLATION_PATH /usr/share/arduino/hardware/archlinux-arduino)

SET(CMAKE_CXX_COMPILER /usr/bin/g++)
set(ARDUINO_SOURCE_PATH ..)
set(HEADERS
#	${ARDUINO_SOURCE_PATH}/config.h
	${ARDUINO_SOURCE_PATH}/util.h
#	${ARDUINO_SOURCE_PATH}/VM.h
	${ARDUINO_SOURCE_PATH}/REPL.h
)

set(SOURCE
#	${ARDUINO_SOURCE_PATH}/VM.cpp
	${ARDUINO_SOURCE_PATH}/REPL.cpp
	${ARDUINO_SOURCE_PATH}/util.cpp
)

include_directories(
#"/usr/share/arduino/hardware/archlinux-arduino/avr/cores/arduino"
#"/usr/avr/include"
#arduino-includes/usr/avr/include
.
${ARDUNIO_INSTALLATION_PATH}
..
##/usr/avr/include
#"/home/gregory/.arduino15/packages/arduino/hardware/avr/1.6.17/cores/arduino"
"/home/gregory/.arduino15/packages/arduino/hardware/avr/1.6.17/variants/standard"
)

add_executable(runavm runavm.cpp ${SOURCE} ${HEADERS})

target_compile_options(runavm PUBLIC -std=gnu++11)

install(TARGETS runavm DESTINATION bin)

set(CMAKE_CXX_FLAGS "-g -Wall -Os -Wall -Wextra -std=gnu++11 -fpermissive -fno-exceptions -ffunction-sections -fdata-sections -fno-threadsafe-statics -MMD -flto")



#

#-mmcu=atmega328p -DF_CPU=16000000L -DARDUINO=10805 -DARDUINO_AVR_UNO -DARDUINO_ARCH_AVR




