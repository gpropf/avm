# AVM
## Arduino Virtual Machine

Welcome to my Arduino virtual machine that runs a compact <s>"modal"</s> machine code. (I ditched the modal aspect when I saw that
it increased code size over what a more complex non-modal instruction set would do and since the critical resource with
the Arduino is the dynamic RAM...

The inspiration for this project came when I saw the need for a form of multitasking on the Arduino. I had been writing
some code on a freelance basis for some people who wanted me to adapt some code they had that ran a bunch of different
Arduino peripherals in repeating sequences of actions. For each peripheral they had a demo program that they wanted to
run along with other similar programs for other peripherals. If you know how the Arduino works, you will know that it
has no operating system and each program (called "sketches" in Arduino parlance) takes complete control of the system.

This created a slight problem - the moment one demo loop started all the others stopped and had to wait their turn.
This is fine if each demo does something very simple and quick but some of the demos were these sort of sequential
scripted affairs that flashed lights, printed something on an LCD display or interacted with the user. My solution
when doing these jobs had simply been to "weave" each demo through the others, alloting a small time slice for each while
maintaining each demo program's state. It worked but was pretty clunky and labor intensive. I started thinking about writing
a simple high level language that targeted an equally simple virtual machine with special purpose instructions for some
of the Arduino's basic IO and configuration tasks. The idea was that the machine would implement a very simple "operation
system" in the form of round-robin processing. Each virtual task runs for a bit, then another, then another, etc... So
maybe I should take the quotes off the OS since we have the basic notion of time slicing and hardware abstraction and management here.
Usually though an OS is written *in* an assembly langauge (or C) whereas this was sort of an "implicit" OS built
into the VM. So whatever you call it, it's definitely a VM.

### This Idea is Impossible and Stupid! 

Of course it is. 

#### The 'Duino has less space than an Atari and you're going to shove an OS *AND SOFTWARE* in there!!!

To that I have a few answers

1) I'm doing this because it amuses me and not because I think it will be profitable or even popular.
2) I've actually surprised myself a little and in about 3 months working very intermittently am at the point where I can run simple programs that take
arguments, return values, run loops, etc... I've got a basic assembler written in Python to help take some of the pain
out of writing machine code.
3) The high level language (which I haven't written yet) will be written in Haskell and is mainly designed to help me learn more about that langauge
and use its powerful Parsec parsing library. None of that runs on the 'Duino. It just targets the VM. I also have some
vague ideas about writing a Haskell program to run on Linux and do code transforms on the fly since...
4) There will be some sort-of cool features like hot-loading of code over the serial line without re-burning the Flash RAM
5) I might also wrap the code up as a CMake project to run on Linux or other larger computers if I think it
would be fun.


