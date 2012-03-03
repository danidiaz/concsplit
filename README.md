This program concatenates a sequence of input files and then splits their contents into a sequence of output files of predetermined size.

This should be done in a single pass, ensuring that file handles are closed the moment they are no longer needed, and in an exception-safe manner (that is, all opened file handles must be properly closed in the event of an exception). 

The program uses iteratees. I guess it would be simpler to implement using conduits or pipes, but I wanted to cement my knowledge of iteratees first. 

Some examples of use:

*   concsplit input1.txt -s 1M

    Split file "input1.txt" into parts of 1 megabyte, using the default method.

*   concsplit input1.txt input2.txt -p "__part" -s 1K13b -m safe1K

    Concatenate "input1.txt" and "input2.txt" and then split the result into parts of 1037 bytes whose filenames start with "__part", using method "safe1K".

*   concsplit -h

    Lists help.

*   concsplit -m

    Lists the available methods for performing the operation.


