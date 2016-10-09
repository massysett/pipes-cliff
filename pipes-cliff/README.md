# Deprecated

I no longer maintain this library.  There may be bugs in it.
Use at your own risk.

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

## Why the name?

It's named after Cliff Clavin, the mailman on the TV show
*Cheers*.  pipes-cliff uses mailboxes to send information to and
from subprocesses.

The obvious name, pipes-process, has already been taken.  At the
time of this writing, I saw at least two libraries with this name,
though neither was posted to Hackage.

## Similar libraries

Take a look at these other libraries; they might meet your needs.

### Dealing specifically with subprocesses and streaming

* process-streaming

http://hackage.haskell.org/package/process-streaming

* pipes-shell

https://hackage.haskell.org/package/pipes-shell

* Data.Conduit.Process

https://www.fpcomplete.com/user/snoyberg/library-documentation/data-conduit-process

### Larger scripting frameworks

* HSH

https://hackage.haskell.org/package/HSH

* Turtle

https://hackage.haskell.org/package/turtle

* Shelly

https://hackage.haskell.org/package/shelly

Also, look at this discussion on the Pipes mailing list:

https://groups.google.com/d/msg/haskell-pipes/JFfyquj5HAg/Lxz7p50JOh4J

## License

This package is released under the BSD3 license. Please see the LICENSE file.

## Building this project

The Cabal file for this project is generated using the Cartel package.
To generate the Cabal file, simply run `sh buildprep`.
You must run this command from the project's main directory.
You will need to have the `stack` program installed.

Stack is available at:
http://www.haskellstack.org
