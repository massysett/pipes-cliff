{-# LANGUAGE RankNTypes #-}
-- | Examples using "Pipes.Cliff".  You will want to look at the
-- source code itself; viewing just the Haddocks will not help you
-- much.  You can view the source using Haddock if you used
-- @--hyperlink-source@ when building the library or if you are
-- viewing this on Hackage; look for the @Source@ link.  Or, you can
-- find the source at
--
-- <https://github.com/massysett/pipes-cliff/blob/master/lib/Pipes/Cliff/Examples.hs>
--
-- __Be sure to use the @-threaded@ option__ when compiling code
-- that uses "Pipes.Cliff", including this code; see the warning in
-- "Pipes.Cliff" for more details.
--
-- Notice throughout how pipelines that move data from one process to
-- another typically are run in the background using 'conveyor', which
-- spawns a thread.  You have to make sure all these threads are
-- running concurrently so that data flows through your pipeline (a
-- shell does this sort of thing for you.)

module Pipes.Cliff.Examples where

import Pipes.Cliff
import qualified Pipes.Prelude as P
import qualified Data.ByteString.Char8 as BS8


-- | Produces a stream of 'BS8.ByteString', where each
-- 'BS8.ByteString' is a shown integer.  This is an infinite stream.
-- In the examples below we'll send this infinite stream off into a
-- Unix pipeline, a feat that would be very difficult and clumsy
-- without a framework like @pipes@.

produceNumbers :: Monad m => Producer BS8.ByteString m r
produceNumbers
  = iterate' succ (0 :: Int)
  >-> P.show >-> P.map BS8.pack >-> P.map (`BS8.snoc` '\n')
  where
    iterate' nxt i = yield i >> iterate' nxt (nxt i)

-- | Streams an infinite list of numbers to @less@.  Shows off how
-- you can use "Pipes.Cliff" even for non-finite 'Producer's.  Don't
-- try to go to the end of the input in @less@, though.  When you
-- quit @less@, you will get broken pipe warnings printed to
-- standard error.  This is normal.  To suppress them, see the
-- 'handler' option.

numsToLess :: IO ExitCode
numsToLess = do
  (toLess, _) <- pipeInput Inherit Inherit (procSpec "less" [])
  safeEffect $ produceNumbers >-> toLess


-- | Streams an infinite list of numbers to @tr@ and then to @less@.
-- Perfectly useless, but shows how to build pipelines.  Also
-- squlches warning messages using the 'handler' option.
--
-- Note that, consistent with usual @pipes@ usage, the value of
-- @code1@ and @code2@ is not necessarily the last exit code in the
-- pipeline.  Rather, it is the exit code of the process that
-- terminated first.  Use 'waitForProcess' if you need to determine
-- the exit value of a particular process.  It's also possible to use
-- a bit of 'fmap' to see which process in a pipeline did terminate
-- first; for an example of that, search the "Pipes.Tutorial" module
-- for @echo3.hs@.
alphaNumbers :: IO (ExitCode, ExitCode)
alphaNumbers = do
  ((toTr, fromTr), _) <- pipeInputOutput Inherit
    (procSpec "tr" ["[0-9]", "[a-z]"]) { handler = squelch }
  (toLess, _) <- pipeInput Inherit Inherit
        (procSpec "less" []) { handler = squelch }
  toTrAsync <- conveyor $ produceNumbers >-> toTr
  toLessAsync <- conveyor $ fromTr >-> toLess
  code1 <- wait toTrAsync
  code2 <- wait toLessAsync
  return (code1, code2)


-- | Produces an infinite stream of numbers, sends it to @tr@ for some
-- mangling, and then to @sh@, which will copy each line both to
-- standard output and to standard error.  From @sh@, standard output
-- is then sent off to @less@, and standard error is sent to a
-- separate thread which will collect the results and return them.
--
-- This example shows you how to write a pipeline that deals with
-- both standard output and standard error.
--
-- It's also interesting to note here that because of the buffering
-- that happens in various places throughout the pipeline, and because
-- less itself also buffers its input, the output you will see from
-- the @sh@ process's standard error will be much longer than the
-- output the user actually viewed in @less@.
standardOutputAndError :: IO BS8.ByteString
standardOutputAndError = do
  ((toTr, fromTr), _) <- pipeInputOutput Inherit
    (procSpec "tr" ["[0-9]", "[a-z]"])
  ((toSh, fromShOut, fromShErr), _) <- pipeInputOutputError
    (procSpec "sh" ["-c", script])
  (toLess, _) <- pipeInput Inherit Inherit (procSpec "less" [])
  _ <- conveyor $ produceNumbers >-> toTr
  _ <- conveyor $ fromTr >-> toSh
  _ <- conveyor $ fromShOut >-> toLess
  runSafeT
    $ P.fold BS8.append BS8.empty id (fromShErr >> return ())
  where
    script = "while read line; do echo $line; echo $line 1>&2; done"


-- | Like 'alphaNumbers' but just sends a limited number
-- of numbers to @cat@.  A useful test to make sure that pipelines
-- shut down automatically.
limitedAlphaNumbers :: IO ExitCode
limitedAlphaNumbers = do
  ((toTr, fromTr), _) <- pipeInputOutput Inherit
    (procSpec "tr" ["[0-9]", "[a-z]"])
  (toCat, _) <- pipeInput Inherit Inherit (procSpec "cat" [])
  _ <- async 
    . safeEffect
    $ produceNumbers
    >-> P.take 300
    >-> (toTr >> return ())
  safeEffect $ fromTr >-> toCat


-- | Produces a finite list of numbers, sends it to @tr@ for some
-- mangling, and then puts the results into a 'BS8.ByteString' for
-- further processing.  This example shows how you can use this
-- library to place the results of a pipeline into a simple strict
-- data type.
alphaNumbersByteString :: IO BS8.ByteString
alphaNumbersByteString = do
  ((toTr, fromTr), _) <- pipeInputOutput Inherit
    (procSpec "tr" ["[0-9]", "[a-z]"])
  _ <- conveyor
    $ produceNumbers
    >-> P.take 300
    >-> (toTr >> return ())
  runSafeT
    $ P.fold BS8.append BS8.empty id (fromTr >> return ())

-- | So far, all examples have ignored the issue of exception safety.
-- Here's an example that properly uses 'bracket' to make sure that
-- all resource allocations are cleaned up if there is an exception.
-- Otherwise, it's identical to 'standardOutputAndError'.  You can put
-- some @do@ notation sugar in here and eliminate all the hanging
-- lambdas and '$'s by using the 'ContT' monad from @transformers@ (I
-- did not write the example that way to avoid incurring a direct
-- dependency on @transformers@).

standardOutputAndErrorBracketed :: IO BS8.ByteString
standardOutputAndErrorBracketed =
  withProcess (pipeInputOutput Inherit (procSpec "tr" ["[0-9]", "[a-z]"]))
    $ \(toTr, fromTr) ->

  withProcess (pipeInputOutputError (procSpec "sh" ["-c", script]))
    $ \(toSh, fromShOut, fromShErr) ->

  withProcess (pipeInput Inherit Inherit (procSpec "less" [])) $ \toLess ->
  withConveyor (produceNumbers >-> toTr) $
  withConveyor (fromTr >-> toSh) $
  withConveyor (fromShOut >-> toLess) $
  runSafeT
    $ P.fold BS8.append BS8.empty id (fromShErr >> return ())
  where
    script = "while read line; do echo $line; echo $line 1>&2; done"
