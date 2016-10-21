# header_parser
C header parser WIP

<pre>
This project "header parser" will take a .ii file (sample included) from:

$ cat >> main.c
#include <stdio.h>
void main(){}
^D
$ cpp main.c main.ii

and produce some debug output.  You can run it like so:

$ ./build.sh
(expect a few errors)
$ cat main.ii | ./parser | less

this project is 80% free of Syntax errors.  Our goals are as follows:

1. eliminate all syntax errors
2. change all debug output to output suited for an intermediary language
3. implement a very basic library for a simple assembler
4. assemble some code for generated macros or functions.
</pre>
