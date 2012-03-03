This program concatenates a sequence of input files and then splits their contents into a sequence of output files of predetermined size.

It should be done in a single pass, ensuring that file handles are closed the moment they are no longer needed, and in an exception-safe manner (that is, all opened file handles must be properly closed in the event of an exception). 

The program uses [iteratees](http://hackage.haskell.org/package/iteratee). I guess it would be simpler to implement using [conduits](http://hackage.haskell.org/package/conduit), [pipes](http://hackage.haskell.org/package/pipes) or [regions](http://hackage.haskell.org/package/safer-file-handles), but I wanted to cement my knowledge of iteratees first. 

Some examples of use:

*   `concsplit input1.txt -s 1M`

    Split file "input1.txt" into parts of 1 megabyte, using the default method.

*   `concsplit input1.txt input2.txt -p "__part" -s 1K13b -m safe1K`

    Concatenate "input1.txt" and "input2.txt" and then split the result into parts of 1037 bytes whose filenames start with "__part", using method "safe1K".

*   `concsplit -h`

    Lists help.

*   `concsplit -m`

    Lists the available methods for performing the operation.

Util.Allocator
--------------

An `Allocator` is a type synonym for a tuple inside an IO action, where the second member of the tuple is of type `IO ()`. The outer IO action performs some type of resource allocation, while the `IO ()` inside the tuple serves to release the previously allocated resource. 

The `combine` function takes a function which knows how to combine the values inside two allocators and also their respective cleanup actions, and returns a function which safely combines two Allocators to form a third.

ConcSplit
---------

This module defines a record type which each provided implementation must fill. One of the members of the record is the `concsplit` function, which has signature: `[Allocator Handle] -> Int -> [Allocator Handle] -> IO ()`

It takes as parameters a list of input files, the desired size of the output files, and a list of output files. This later list is assumed to be infinite: new output files will be allocated as needed.

ConcSplit.Leaky
---------------

This module contains a naive implementation which doesn't quite work. Among other problems, output file handles allocated by the iteratee are not properly closed when the enumerator encounters an exception.

Also, when the enumerator concludes, the file to which the iteratee is currently writing remains open.

ConcSplit.Safe
--------------

This implementation should work better (or so I hope!). It properly closes the last output file and it doesn't leave open handles after encountering a synchronous or asynchronous exception. It is the default method.

To guard against asynchronous exceptions, resource allocations are performed while masked, and the mask is only dropped once the necessary exception handlers have been put in place and we are ready to read/write from the current handles.


