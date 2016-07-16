-- | Spawn subprocesses and interact with them using "Pipes"
--
-- The interface in this module deliberately resembles the interface
-- in "System.Process".  However, one consequence of this is that you
-- will not want to have unqualified names from this module and from
-- "System.Process" in scope at the same time.
--
-- As in "System.Process", you create a subprocess by creating a
-- 'CreateProcess' record and then applying a function to that
-- record.  Unlike "System.Process", you use functions such as
-- 'pipeInput' or 'pipeInputOutput' to specify what streams you want
-- to use a 'Proxy' for and what streams you wish to be 'Inherit'ed
-- or if you want to 'UseHandle'.  You then send or receive
-- information using one or more 'Proxy'.
--
-- __Use the @-threaded@ GHC option__ when compiling your programs or
-- when using GHCi.  Internally, this module uses
-- 'System.Process.waitForProcess' from the "System.Process" module.
-- As the documentation for 'waitForProcess' states, you must use the
-- @-threaded@ option to prevent every thread in the system from
-- suspending when 'waitForProcess' is used.  So, if your program
-- experiences deadlocks, be sure you used the @-threaded@ option.
--
-- This module relies on the "Pipes", "Pipes.Safe",
-- "Control.Concurrent.Async", and "System.Process" modules.  You will
-- want to have basic familiarity with what all of those modules do
-- before using this module.
--
-- All communcation with subprocesses is done with strict
-- 'ByteString's.  If you are dealing with textual data, the @text@
-- library has functions to convert a 'ByteString' to a @Text@; you
-- will want to look at @Data.Text.Encoding@.
--
-- Nobody would mistake this module for a shell; nothing beats the
-- shell as a language for starting other programs, as the shell is
-- designed for that.  This module allows you to perform simple
-- streaming with subprocesses without leaving the comfort of Haskell.
-- Take a look at the README.md file, which is distributed with the
-- tarball or is available at Github at
--
-- <https://github.com/massysett/pipes-cliff>
--
-- There you will find references to other libraries that you might
-- find more useful than this one.
--
-- You will want to consult "Pipes.Cliff.Examples" for some examples
-- before getting started.  There are some important notes in there
-- about how to run pipelines.
module Pipes.Cliff
  ( -- * Specifying a subprocess's properties
    CmdSpec(..)
  , NonPipe(..)
  , CreateProcess(..)
  , procSpec
  , squelch

  -- * Creating processes
  -- $process
  
  , pipeInput
  , pipeOutput
  , pipeError
  , pipeInputOutput
  , pipeInputError
  , pipeOutputError
  , pipeInputOutputError

  -- * 'Proxy' combinators
  , conveyor
  , safeEffect

  -- * Querying and terminating the process
  , ProcessHandle
  , originalCreateProcess
  , isStillRunning
  , waitForProcess
  , terminateProcess
  
  -- * Exception safety

  -- | These are some simple combinators built with
  -- 'Control.Exception.bracket'; feel free to use your own favorite
  -- idioms for exception safety.
  , withProcess
  , withConveyor

  -- * Errors and warnings

  -- | You will only need what's in this section if you want to
  -- examine errors more closely.
  , Activity(..)
  , Outbound(..)
  , HandleDesc(..)
  , Oopsie(..)

  -- * Re-exports
  -- $reexports
  , module Control.Concurrent.Async
  , module Pipes
  , module Pipes.Safe
  , module System.Exit

  -- * Some design notes
  -- $designNotes
  ) where

import Control.Concurrent.Async
import Pipes.Cliff.Core
import Pipes
import Pipes.Safe (runSafeT)
import System.Exit

{- $process

Each of these functions creates a process.  The process begins
running immediately in a separate process while your Haskell program
continues concurrently.  A function is provided for each possible
combination of standard input, standard output, and standard error.
Use the 'NonPipe' type to describe what you want to do with streams
you do NOT want to create a stream for.  For example, to create a
subprocess that creates a 'Proxy' for standard input and standard
output, use 'pipeInputOutput'.  You must describe what you want done
with standard error.  A 'Producer' is returned for standard output
and a 'Consumer' for standard input.

Each function also returns a 'ProcessHandle'; this is not the same
'ProcessHandle' that you will find in "System.Process".  You can use
this 'ProcessHandle' to obtain some information about the process that
is created and to get the eventual 'ExitCode'.

Every time you create a process with one of these functions, some
additional behind-the-scenes resources are created, such as some
threads to move data to and from the process.  In normal usage, these
threads will be cleaned up after the process exits.  However, if
exceptions are thrown, there could be resource leaks.  Applying
'terminateProcess' to a 'ProcessHandle' makes a best effort to clean
up all the resources that Cliff may create, including the process
itself and any additional threads.  To guard against resource leaks,
use the functions found in "Control.Exception" or in
"Control.Monad.Catch".  "Control.Monad.Catch" provides operations that
are the same as those in "Control.Exception", but they are not limited
to 'IO'.

I say that 'terminateProcess' \"makes a best effort\" to release
resources because in UNIX it merely sends a @SIGTERM@ to the process.
That should kill well-behaved processes, but 'terminateProcess' does
not send a @SIGKILL@.  'terminateProcess' always closes all handles
associated with the process and it kills all Haskell threads that were
moving data to and from the process.  ('terminateProcess' does not
kill threads it does not know about, such as threads you created with
'conveyor'.)

There is no function that will create a process that has no 'Proxy'
at all.  For that, just use 'System.Process.createProcess' in
"System.Process".

-}

{- $reexports

   * "Control.Concurrent.Async" reexports all bindings

   * "Pipes" reexports all bindings

   * "Pipes.Safe" reexports 'runSafeT'

   * "System.Exit" reexports all bindings

-}

{- $designNotes

Two overarching principles guided the design of this
library. First, I wanted the interface to use simple
ByteStrings.  That most closely represents what a UNIX process
sees.  If the user wants to use Text or String, it's easy
enough to convert between those types and a ByteString.  Then
the user has to pay explicit attention to encoding issues--as
she should, because not all UNIX processes deal with encoded
textual data.

Second, I paid meticulous attention to resource management.  Resources
are deterministically destroyed immediately after use.  This
eliminates many bugs.  Even so, I decided to leave it up to the user
to use something like 'Control.Exception.bracket' to ensure that all
resources are cleaned up if there is an exception.  Originally I tried
to have the library do this, but that turned out not to be very
composable.  There are already many exception-handling mechanisms
available in "Control.Exception", "Pipes.Safe", and
"Control.Monad.Catch", and it seems best to let the user choose how to
handle this issue; she can just perform a 'Control.Exception.bracket'
and may combine this with the @ContT@ monad in @transformers@ or @mtl@
if she wishes, or perhaps with the @managed@ library.

An earlier version of this library (see version 0.8.0.0) tried to use
the return value of a 'Proxy' to indicate the return value of both
processes in the pipeline, not just one.  I removed this because it
interfered heavily with composability.

You might wonder why, if you are using an external process as
a pipeline, why can't you create, well, a 'Pipe'?  Wouldn't
that be an obvious choice?  Well, if such an interface is
possible using Pipes in its current incarnation, nobody has
figured it out yet.  I don't think it's possible.  See also

<https://groups.google.com/d/msg/haskell-pipes/JFfyquj5HAg/Lxz7p50JOh4J>

for a discussion about this.

-}
