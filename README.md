# pipes-cliff

pipes-cliff helps you spawn subprocesses and send data to and
from them with the Pipes library.  Subprocesses are opened using the
process library, and the processes and handles are properly cleaned
up even if there are exceptions.

Though this library uses the Pipes library, I have not coordinated
with the author of the Pipes library in any way.  Any bugs or design
flaws are mine and should be reported to

http://www.github.com/massysett/pipes-cliff/issues

Though I hope pipes-cliff works on Windows systems, I have only
tested it on Unix-like systems.  Any reports on how this library works
on Windows are appreciated.

## Building pipes-cliff

If you get the files from Hackage, just build them in the usual way
using cabal-install.

To build the files from Github, you will need to generate the Cabal
file.  The Cabal file is generated using the Cartel package, so first
run

cabal install cartel

and then, from the main directory (the one with the generate script),
run

sh generate

(at least, this is how you would do it on a Unix-like system.)

If you are having trouble getting pipes-cliff to build, examine the
build history here:

[![Build Status](https://travis-ci.org/massysett/pipes-cliff.svg?branch=master)](https://travis-ci.org/massysett/pipes-cliff)

Previous builds will show you the dependencies that were used in the
past to build the library.

## Why the name?

It's named after Cliff Clavin, the mailman on the TV show
*Cheers*.  pipes-cliff uses mailboxes to send information to and
from subprocesses.

The obvious name, pipes-process, has already been taken.  At the
time of this writing, I saw at least two libraries with this name,
though neither was posted to Hackage.

## Similar libraries

Take a look at these other libraries; they might meet your needs.

### Focused on subprocesses generally

* process-streaming

http://hackage.haskell.org/package/process-streaming

* pipes-shell

https://hackage.haskell.org/package/pipes-shell

### Larger scripting frameworks

* HSH

https://hackage.haskell.org/package/HSH

* Turtle

https://hackage.haskell.org/package/turtle

* Shelly

https://hackage.haskell.org/package/shelly

Also, look at this discussion on the Pipes mailing list:

https://groups.google.com/d/msg/haskell-pipes/JFfyquj5HAg/Lxz7p50JOh4J
