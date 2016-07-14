{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- | This contains the innards of Cliff.
-- You shouldn't need anything that's in this module; instead, use
-- "Pipes.Cliff".
--
-- Exit code and waiting for processes: as of base 4.7, there was a
-- bug in 'System.Process.waitForProcess' which may arise if you have
-- multiple threads waiting for a single process to finish.  Thus this
-- module is set up so that only one thread does the wait, and it
-- places the result in an MVar.  See
--
-- http://ghc.haskell.org/trac/ghc/ticket/9292

module Pipes.Cliff.Core where

import System.Environment
import Data.List (intersperse)
import Control.Exception (IOException)
import System.IO
import qualified System.Process as Process
import Pipes
import Pipes.Safe
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import System.Exit
import qualified Control.Exception
import Control.Monad
import Control.Concurrent.STM

-- * Data types

-- | Like 'Process.CmdSpec' in "System.Process", but also has an
-- instance for 'Show'.
data CmdSpec
  = ShellCommand String
  | RawCommand FilePath [String]
  deriving (Eq, Ord, Show)

convertCmdSpec :: CmdSpec -> Process.CmdSpec
convertCmdSpec (ShellCommand s) = Process.ShellCommand s
convertCmdSpec (RawCommand p ss) = Process.RawCommand p ss

-- ** Errors

-- | When dealing with a 'Handle', errors can occur when reading from,
-- writing to, or closing the handle.
data Activity
  = Reading
  | Writing
  | Closing
  deriving (Eq, Ord, Show)

-- | The two kinds of outbound handles.
data Outbound
  = Output
  | Error
  deriving (Eq, Ord, Show)

-- | Describes a handle.  From the perspective of the subprocess; for
-- example, 'Input' means that this handle is connected to the
-- process's standard input.

data HandleDesc
  = Input
  | Outbound Outbound
  deriving (Eq, Ord, Show)

-- | Describes all IO exceptions.  The 'Oopsie' contains the
-- 'IOException' itself, along with the 'CmdSpec' that was running
-- when the exception occurred.
--
-- The exceptions that are caught and placed into an 'Oopsie' may
-- arise from reading data from or writing data to a 'Handle'.  In
-- these errors, the associated 'Producer' or 'Consumer' will
-- terminate (which may trigger various cleanup actions in the
-- 'MonadSafe' computation) but the exception itself is not
-- re-thrown; rather, it is passed to the 'handler'.  Similarly, an
-- exception may occur while closing a handle; these exceptions are
-- caught, not rethrown, and are passed to the 'handler'.  If an
-- exception arises when terminating a process (I'm not sure this is
-- possible) then it is also caught, not rethrown, and passed to the
-- 'handler'.
--
-- If an exception arises when creating a process--such as a command
-- not being found--the exception is /not/ caught, handled, or
-- passed to the 'handler'.  In addition, no exceptions are caught
-- if they originated during a 'Process.waitForProcess'.  (I can't
-- conceive of how any synchronous exceptions could arise from
-- 'Process.waitForProcess', but if they do, Cliff does not handle
-- them.)  Also, an 'Oopsie' is created only for an 'IOException';
-- no other exceptions of any kind are caught or handled.  However,
-- exceptions of any kind will still trigger appropriate cleanup
-- actions in the 'MonadSafe' computation.
data Oopsie = Oopsie Activity HandleDesc CmdSpec IOException
  deriving (Eq, Show)

-- | Formats an 'Oopsie' for display.
renderOopsie
  :: String
  -- ^ The name of the currently runnning program
  -> Oopsie
  -> String
renderOopsie pn (Oopsie act desc cmd ioe) =
  pn ++ ": warning: when running command "
  ++ renderCommand cmd ++ ": " ++ renderHan
  ++ ": " ++ show ioe
  where
    renderCommand (ShellCommand str) = show str
    renderCommand (RawCommand fp ss)
      = concat . intersperse " " . map show
      $ fp : ss

    renderHan =
      "when " ++ actStr ++ " " ++ descStr
      where
        actStr = case act of
          Reading -> "reading from"
          Writing -> "writing to"
          Closing -> "closing the handle associated with"
        descStr = "standard " ++ case desc of
          Input -> "input"
          Outbound Output -> "output"
          Outbound Error -> "error"

-- | The default handler when receiving an 'Oopsie'; simply uses
-- 'renderOopsie' to format it nicely and put it on standard error.
--
-- Side effects: gets the program name from the environment, and
-- prints the Oopsie to standard error.
defaultHandler :: Oopsie -> IO ()
defaultHandler oops = do
  pn <- getProgName
  hPutStrLn stderr $ renderOopsie pn oops

-- ** Configuration types

-- | How will the subprocess get its information for this stream?  A
-- 'NonPipe' is used for streams that will not be assigned to a
-- 'Proxy' but, instead, will be inherited from the parent or directed
-- from an existing 'Handle'.
data NonPipe
  = Inherit
  -- ^ Use whatever stream that the parent process has.
  | UseHandle Handle
  -- ^ Use the given handle for input or output

convertNonPipe :: Maybe NonPipe -> Process.StdStream
convertNonPipe a = case a of
  Nothing -> Process.CreatePipe
  Just Inherit -> Process.Inherit
  Just (UseHandle h) -> Process.UseHandle h

-- | Like 'System.Process.CreateProcess' in "System.Process",
-- this gives the necessary information to create a subprocess.  All
-- but one of these fields is also present in
-- 'System.Process.CreateProcess', and they all have the same meaning;
-- the only field that is different is the 'handler' field.
data CreateProcess = CreateProcess
  { cmdspec :: CmdSpec
    -- ^ Executable and arguments, or shell command

  , cwd :: Maybe FilePath
  -- ^ A new current working directory for the subprocess; if
  -- 'Nothing', use the calling process's working directory.

  , env :: Maybe [(String, String)]
  -- ^ The environment for the subprocess; if 'Nothing', use the
  -- calling process's working directory.

  , close_fds :: Bool
  -- ^ If 'True', close all file descriptors other than the standard
  -- descriptors.  See the documentation for
  -- 'System.Process.close_fds' for details on how this works in
  -- Windows.

  , create_group :: Bool
  -- ^ If 'True', create a new process group.

  , delegate_ctlc :: Bool
  -- ^ See 'System.Process.delegate_ctlc' in the "System.Process"
  -- module for details.

  , handler :: Oopsie -> IO ()
  -- ^ Whenever an IO exception arises during the course of various
  -- IO actions, the exception is caught and placed into an 'Oopsie'
  -- that indicates why and where the exception happened.  The
  -- 'handler' determines what happens when an 'Oopsie' comes in.
  -- See 'Oopsie' for details.
  --
  -- The default 'handler' created by 'procSpec' is
  -- 'defaultHandler', which will simply print the exceptions to
  -- standard error.  You may not want to see the exceptions at all.
  -- For example, many exceptions come from broken pipes.  A broken
  -- pipe might be entirely normal in your circumstance.  For
  -- example, if you are streaming a large set of values to a pager
  -- such as @less@ and you expect that the user will often quit the
  -- pager without viewing the whole result, a broken pipe will
  -- result, which will print a warning message.  That can be a
  -- nuisance.
  --
  -- If you don't want to see the exceptions at all, just set
  -- 'handler' to 'squelch', which simply discards the exceptions.
  --
  -- Conceivably you could rig up an elaborate mechanism that puts
  -- the 'Oopsie's into a "Pipes.Concurrent" mailbox or something.
  -- Indeed, when using 'defaultHandler' each thread will print its
  -- warnings to standard error at any time.  If you are using
  -- multiple processes and each prints warnings at the same time,
  -- total gibberish can result as the text gets mixed in.  You
  -- could solve this by putting the errors into a
  -- "Pipes.Concurrent" mailbox and having a single thread print the
  -- errors; this sort of thing could be built into the library but
  -- so far I haven't been motivated to do it.

  }

-- | Do not show or do anything with exceptions; useful to use as a
-- 'handler'.
--
-- Side effects: None.
squelch :: Monad m => a -> m ()
squelch = const (return ())

-- | Create a 'CreateProcess' record with default settings.  The
-- default settings are:
--
-- * a raw command (as opposed to a shell command) is created
--
-- * the current working directory is not changed from the parent process
--
-- * the environment is not changed from the parent process
--
-- * the parent's file descriptors (other than standard input,
-- standard output, and standard error) are inherited
--
-- * no new process group is created
--
-- * 'delegate_ctlc' is 'False'
--
-- * 'handler' is 'defaultHandler'

procSpec
  :: String
  -- ^ The name of the program to run, such as @less@.
  -> [String]
  -- ^ Command-line arguments
  -> CreateProcess
procSpec prog args = CreateProcess
  { cmdspec = RawCommand prog args
  , cwd = Nothing
  , env = Nothing
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  , handler = defaultHandler
  }

convertCreateProcess
  :: Maybe NonPipe
  -> Maybe NonPipe
  -> Maybe NonPipe
  -> CreateProcess
  -> Process.CreateProcess
convertCreateProcess inp out err a = Process.CreateProcess
  { Process.cmdspec = convertCmdSpec $ cmdspec a
  , Process.cwd = cwd a
  , Process.env = env a
  , Process.std_in = conv inp
  , Process.std_out = conv out
  , Process.std_err = conv err
  , Process.close_fds = close_fds a
  , Process.create_group = create_group a
  , Process.delegate_ctlc = delegate_ctlc a
  , Process.new_session = False
  , Process.child_user   = Nothing
  , Process.child_group  = Nothing
  , Process.detach_console = False
  , Process.create_new_console = False
  }
  where
    conv = convertNonPipe

-- * MVar types

-- ** Lock

-- | Guarantees single-thread access
--
-- All MVar idioms thanks to Neil Mitchell:
-- <http://neilmitchell.blogspot.com/2012/06/flavours-of-mvar_04.html>
type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

withLock ::  Lock -> IO a -> IO a
withLock x = withMVar x . const

-- ** Var

-- | Operates on mutable variables in thread-safe way.

type Var a = MVar a

newVar :: a -> IO (Var a)
newVar = newMVar

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar = modifyMVar

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ = modifyMVar_

readVar :: Var a -> IO a
readVar = readMVar

-- ** Barrier

-- | Starts with no value, is written to once, and is read one or
-- more times.
type Barrier a = MVar a

newBarrier :: IO (Barrier a)
newBarrier = newEmptyMVar

signalBarrier :: Barrier a -> a -> IO ()
signalBarrier = putMVar

waitBarrier :: Barrier a -> IO a
waitBarrier = readMVar

-- ** MVar abstractions


-- | Takes an action and returns a new action.  If the action is
-- never called the argument action will never be executed, but if
-- it is called more than once, it will only be executed once.
--
-- Side effects: creates a 'Var'.  Returns an IO action that modifies
-- the contents of that 'Var'.
once :: IO a -> IO (IO a)
once act = do
  var <- newVar Nothing
  return $ join $ modifyVar var $ \v -> case v of
    Nothing -> do
      b <- newBarrier
      let r = do
            x <- act
            signalBarrier b x
            return x
      return (Just b, r)
    Just b -> return (Just b, waitBarrier b)
    
-- * Mailboxes

-- | Creates a new mailbox.  Returns an action to send to the mailbox;
-- this action will return False if the mailbox is sealed, or True if
-- the message was successfully placed in the mailbox.  Also returns
-- an action to retrieve from the mailbox, which returns Nothing if
-- the mailbox is sealed, or Just if there is a value to be retrieved.
-- Also returns an action to seal the mailbox.
messageBox :: IO (a -> STM Bool, STM (Maybe a), STM ())
messageBox = atomically $ do
  locked <- newTVar False
  mailbox <- newEmptyTMVar
  return (sendBox locked mailbox, recvBox locked mailbox, sealer locked)
  
sendBox :: TVar Bool -> TMVar a -> a -> STM Bool
sendBox locked mailbox a = do
  isLocked <- readTVar locked
  if isLocked
    then return False
    else do
      putTMVar mailbox a
      return True

recvBox :: TVar Bool -> TMVar a -> STM (Maybe a)
recvBox locked mailbox = do
  mayA <- tryTakeTMVar mailbox
  case mayA of
    Just a -> return $ Just a
    Nothing -> do
      isLocked <- readTVar locked
      if isLocked
        then return Nothing
        else retry
    
sealer :: TVar Bool -> STM ()
sealer locked = writeTVar locked True

produceFromBox :: MonadIO m => STM (Maybe a) -> Producer a m ()
produceFromBox stm = do
  mayV <- liftIO $ atomically stm
  case mayV of
    Nothing -> return ()
    Just v -> yield v >> produceFromBox stm
    
sendToBox :: MonadIO m => (a -> STM Bool) -> Consumer a m ()
sendToBox stm = do
  v <- await
  r <- liftIO $ atomically (stm v)
  if r then sendToBox stm else return ()

-- * Console

-- | Data that is computed once, after the process has been created.
-- After computation, this data does not change.
data Console = Console
  { csIn :: Maybe Handle
  -- ^ Standard input
  , csOut :: Maybe Handle
  -- ^ Standard output
  , csErr :: Maybe Handle
  -- ^ Standard error
  , csHandle :: Process.ProcessHandle
  , csExitCode :: IO ExitCode
  -- ^ IO action that will return the exit code.  Use this rather than
  -- using 'Process.waitForProcess' on the 'csHandle'.
  , csLock :: Lock
  -- ^ If locked, new resources cannot be created.  Obtain this lock
  -- while registering new releasers in 'csReleasers'.
  , csReleasers :: Var [IO ()]
  -- ^ Each time a resource is created, register a finalizer here.
  -- These finalizers are run when 'terminateProcess' is run.
  }
  

-- | Is this process still running?
--
-- Side effects: examines the process handle to see if it has yet
-- returned a value.  Does not block; should return immediately.
isStillRunning :: ProcessHandle -> IO Bool
isStillRunning ph = do
  cnsl <- phConsole ph
  cd <- Process.getProcessExitCode (csHandle cnsl)
  return . maybe True (const False) $ cd

-- | Allows you to terminate the process, as well as to obtain some
-- information about the process.
data ProcessHandle = ProcessHandle
  { phCreateProcess :: CreateProcess
  , phConsole :: IO Console
  }
  
-- | Tells you the 'CreateProcess' that was originally used to create
-- the process associated with this 'ProcessHandle'.
originalCreateProcess :: ProcessHandle -> CreateProcess
originalCreateProcess = phCreateProcess
  
-- | Add a finalizer to the ProcessHandle.  When the finalizers are run, all
-- exceptions are ignored, except asynchronous exceptions, which are
-- masked.
addReleaser :: ProcessHandle -> IO () -> IO ()
addReleaser pnl rel = do
  cnsl <- phConsole pnl
  withLock (csLock cnsl) $
    modifyVar_ (csReleasers cnsl) (\ls -> return (rel : ls))

-- | Terminates a process.  Sends the process a @SIGTERM@, which does
-- not absolutely guarantee that it will exit.  Closes any 'Handle's
-- that were created for the process through Cliff, and terminates any
-- associated background threads that were moving data to and from the
-- process.  Use this function this with 'Control.Exception.bracket'
-- to ensure proper cleanup of resources.
terminateProcess :: ProcessHandle -> IO ()
terminateProcess pnl = mask_ $ do
  cnsl <- phConsole pnl
  withLock (csLock cnsl) $ do
    let runFnlzr fnl = fnl `catch` catcher
        catcher e = return ()
          where _types = e :: Control.Exception.SomeException
    fnlzrs <- readVar (csReleasers cnsl)
    mapM_ runFnlzr fnlzrs

-- | Gets the exit code of the process that belongs to the 'ProcessHandle'.
-- Often you can get the exit code through more idiomatic @pipes@
-- functions, as the various 'Proxy' return the 'ExitCode'.  Sometimes
-- though it can be difficult to use the @pipes@ idioms to get the
-- exit code, so this function is here.
--
-- Side effects: may block if process has not yet exited.
waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess pnl = phConsole pnl >>= csExitCode

-- | Creates a new ProcessHandle.
--
-- Side effects: Does not create the process right away;
-- instead, creates an IO action that, when run, will create the
-- process.  This IO action contains another IO action that, when run,
-- will return the process exit code.
--
-- In addition, the IO action will fork a simple thread that will
-- immediately wait for the process.  In effect, this means there is
-- immediately a thread that will wait for the process to exit.
-- Because this IO action was created with 'once', that means only
-- one thread ever does the @wait@, which avoids a bug in
-- "System.Process".
newProcessHandle
  :: Maybe NonPipe
  -> Maybe NonPipe
  -> Maybe NonPipe
  -> CreateProcess
  -> IO ProcessHandle
newProcessHandle inp out err cp = liftM2 ProcessHandle (return cp) (once act)
  where
    act = mask_ $ do
      (inp', out', err', han) <- Process.createProcess
        (convertCreateProcess inp out err cp)
      let killHan mayH desc = case mayH of
            Nothing -> return ()
            Just h -> closeHandleNoThrow h desc (cmdspec cp) (handler cp)
          destroyers =
            [ killHan inp' Input, killHan out' (Outbound Output),
              killHan err' (Outbound Error),
              Process.terminateProcess han ]
      getCode <- once $ Process.waitForProcess han
      _ <- forkIO (getCode >> return ())
      lock <- newLock
      rlsrs <- newVar destroyers
      return $ Console inp' out' err' han getCode lock rlsrs
      

-- * Exception handling

-- | Sends an exception using the exception handler specified in the
-- 'ErrSpec'.  Side effects: transmits the 'Oopsie' to the right
-- place; the recipient of the 'Oopsie' might have additional side
-- effects.
handleException
  :: Activity
  -> HandleDesc
  -> CmdSpec
  -> (Oopsie -> IO ())
  -> IOException
  -> IO ()
handleException act desc spec sender exc = sender oops
  where
    oops = Oopsie act desc spec exc

-- | Close a handle.  Catches any exceptions and passes them to the handler.
closeHandleNoThrow
  :: Handle
  -> HandleDesc
  -> CmdSpec
  -> (Oopsie -> IO ())
  -> IO ()
closeHandleNoThrow hand desc spec hndlr
  = (hClose hand) `catch`
    (handleException Closing desc spec hndlr)


-- * Threads

-- | Runs in the background an effect, typically one that is moving
-- data from one process to another.  For examples of its usage, see
-- "Pipes.Cliff.Examples".
conveyor :: Effect (SafeT IO) a -> IO (Async a)
conveyor = async . runSafeT . runEffect

-- * Effects

-- | Runs in the foreground an effect in the 'SafeT' monad.
safeEffect :: Effect (SafeT IO) a -> IO a
safeEffect = runSafeT . runEffect


-- * Mailboxes

-- | Creates a new mailbox and returns 'Proxy' that stream values
-- into and out of the mailbox.  Each 'Proxy' is equipped with a
-- finalizer that will seal the mailbox immediately after production
-- or consumption has completed, even if such completion is not due
-- to an exhausted mailbox.  This will signal to the other side of
-- the mailbox that the mailbox is sealed.
--
-- Also returns an STM action to seal the box manually.
newMailbox
  :: (MonadSafe mi, MonadSafe mo)
  => IO (Consumer a mi (), Producer a mo (), STM ())
newMailbox = do
  (toBox, fromBox, seal) <- messageBox
  let csmr = register (liftIO $ atomically seal)
             >> sendToBox toBox
      pdcr = register (liftIO $ atomically seal)
             >> produceFromBox fromBox
  return (csmr, pdcr, seal)

-- * Exception safety

-- | Creates a process, uses it, and terminates it when the last
-- computation ends.  Don't try to use any of the process resources
-- after the last computation ends, because the process will already
-- have been terminated.  For an example of its use, see
-- 'Pipes.Cliff.Examples.standardOutputAndErrorBracketed'.
withProcess
  :: IO (a, ProcessHandle)
  -- ^ Creates the process
  -> (a -> IO b)
  -- ^ Uses the process
  -> IO b
withProcess acq use = Control.Exception.bracket acq (terminateProcess . snd)
  (use . fst)

-- | Runs an 'Effect' in the backgroud (typically one that is moving
-- data from one process to another).  If the background thread is
-- still running when the second computation ends, the background
-- thread is terminated.  For an example of its use, see
-- 'Pipes.Cliff.Examples.standardOutputAndErrorBracketed'.

withConveyor
  :: Effect (SafeT IO) a
  -- ^ The 'Effect' to run in another thread
  -> IO b
  -- ^ The rest of the computation to run
  -> IO b
withConveyor cvy end = Control.Exception.bracket (conveyor cvy) cancel
  (\_ -> end)

-- * Production from and consumption to 'Handle's

-- | I have no idea what this should be.  I'll start with a simple
-- small value and see how it works.
bufSize :: Int
bufSize = 1024


-- | Initialize a handle.  Returns a computation in the MonadSafe
-- monad.  That computation has a registered finalizer that will close
-- a particular handle that is found in the 'ProcessHandle'.  As a side
-- effect, the IO action creating the 'ProcessHandle' is viewed, meaning that
-- the process will launch if it hasn't already done so.
initHandle
  :: (MonadSafe mi, MonadCatch (Base mi))
  => HandleDesc
  -- ^ Used for error messages
  -> (Console -> Handle)
  -- ^ Fetch the handle to close from the 'ProcessHandle'.
  -> ProcessHandle
  -- ^ Has the 'Handle' that will be closed.
  -> (Handle -> mi a)
  -- ^ The remainder of the computation.
  -> mi a
initHandle desc get pnl mkProxy = mask_ $ do
  cnsl <- liftIO $ phConsole $ pnl
  mask $ \restore ->
    let han = get cnsl
        fnlzr = closeHandleNoThrow han desc (cmdspec . phCreateProcess $ pnl)
          (handler . phCreateProcess $ pnl)
    in register (liftIO fnlzr) >> (restore $ mkProxy han)

-- Returns a Consumer for process standard input.
--
-- Side effects: Process is started if it isn't already.  The returned
-- computation will await values and pass them on to the process
-- standard input mailbox.  Any IO exceptions are caught, and
-- consumption terminates.
--
-- I would rather just catch broken pipe exceptions, but I'm not sure
-- there is a good way to do that.
consumeToHandle
  :: (MonadSafe mi, MonadCatch (Base mi))
  => ProcessHandle
  -> Consumer ByteString mi ()
consumeToHandle pnl = initHandle Input get pnl fn
  where
    get cnsl = case csIn cnsl of
      Just h -> h
      Nothing -> error "consumeToHandle: handle not initialized"
    fn han = do
      let hndlr = liftIO . handleException Writing Input
            (cmdspec . phCreateProcess $ pnl)
            (handler . phCreateProcess $ pnl)
          go = do
            bs <- await
            liftIO $ BS.hPut han bs
            go
      go `catch` hndlr

-- | Produce values from a process standard output.  Process is
-- started if it isn't already.
produceFromHandle
  :: (MonadSafe mi, MonadCatch (Base mi))
  => Outbound
  -> ProcessHandle
  -> Producer ByteString mi ()
produceFromHandle outb pnl = initHandle (Outbound outb) get pnl fn
  where
    get cnsl = case outb of
      Output -> case csOut cnsl of
        Nothing -> error "produceFromHandle: stdout not initialized"
        Just h -> h
      Error -> case csErr cnsl of
        Nothing -> error "produceFromHandle: stderr not initialized"
        Just h -> h
    fn han =
      let hndlr = liftIO . handleException Reading (Outbound outb)
            (cmdspec . phCreateProcess $ pnl)
            (handler . phCreateProcess $ pnl)
          go bs
            | BS.null bs = return ()
            | otherwise = yield bs >> produce
          produce = liftIO (BS.hGetSome han bufSize) >>= go
      in produce `catch` hndlr


-- | Given an 'Async', waits for that thread to finish processing
-- values.  When it completes, wait for the process exit code.
finishProxy
  :: Async ()
  -> ProcessHandle
  -> IO ExitCode
finishProxy thread pnl = do
  _ <- wait thread
  waitForProcess pnl
  
-- | Takes all steps necessary to get a 'Consumer' for standard
-- input:
--
-- * Creates a 'Consumer' that will consume to the process standard
-- input.  This 'Consumer' registers a MonadSafe releaser that will
-- close the handle.
--
-- * Creates a mailbox, with a 'Producer' from the mailbox and a
-- 'Consumer' to the mailbox.  Each of these 'Proxy' has a MonadSafe
-- releaser that will close the mailbox.
--
-- * Spwans a thread to run an 'Effect' that connects the 'Consumer'
-- that is connected to the handle to the 'Producer' from the mailbox.
-- In a typical UNIX pipeline situation (where the process keeps its
-- stdin open as long as it is getting input) this 'Effect' will stop
-- running only when the mailbox is sealed.
--
-- * Registers a releaser in the Panel (not in the MonadSafe
-- computation) to destroy the thread; this is in case the user
-- terminates the process.
--
-- * Returns a 'Consumer'.  The 'Consumer' consumes to the mailbox.
-- This 'Consumer' returns the exit code of this process (but remember
-- that the ultimate result of the 'Proxy' depends on which component
-- terminates first).
--
-- Does not register in the 'MonadSafe' an action to cancel the
-- background thread.  Data might still be moving to the process even
-- if the 'Proxy' has shut down.  Let the thread terminate through
-- mailbox closure or a broken pipe.
runInputHandle
  :: (MonadSafe mi, MonadCatch (Base mi))
  => ProcessHandle
  -- ^
  -> Consumer ByteString mi ExitCode
  -- ^
runInputHandle pnl = mask $ \restore -> do
  (toBox, fromBox, seal) <- liftIO newMailbox
  asyncId <- liftIO . conveyor $ fromBox >-> (consumeToHandle pnl)
  liftIO $ addReleaser pnl (cancel asyncId)
  restore $ do
    toBox
    liftIO $ atomically seal
    liftIO $ finishProxy asyncId pnl


-- | Takes all steps necessary to get a 'Producer' for standard
-- input.  Sets up a mailbox, runs a conveyor in the background.  Then
-- receives streaming data, and then gets the process exit code.
runOutputHandle
  :: (MonadSafe mi, MonadCatch (Base mi))
  => Outbound
  -- ^
  -> ProcessHandle
  -- ^
  -> Producer ByteString mi ExitCode
  -- ^
runOutputHandle outb pnl = mask $ \restore -> do
  (toBox, fromBox, seal) <- liftIO newMailbox
  asyncId <- liftIO . conveyor $ (produceFromHandle outb pnl) >-> toBox
  liftIO $ addReleaser pnl (cancel asyncId)
  restore $ do
    fromBox
    liftIO $ atomically seal
    liftIO $ finishProxy asyncId pnl


-- * Creating Proxy

-- | Create a 'Consumer' for standard input.
pipeInput
  :: (MonadSafe mi, MonadCatch (Base mi))

  => NonPipe
  -- ^ Standard output

  -> NonPipe
  -- ^ Standard error

  -> CreateProcess

  -> IO (Consumer ByteString mi ExitCode, ProcessHandle)
  -- ^ A 'Consumer' for standard input
pipeInput out err cp = mask_ $ do
  pnl <- newProcessHandle Nothing (Just out) (Just err) cp
  let inp = runInputHandle pnl
  return (inp, pnl)
    

-- | Create a 'Producer' for standard output.
pipeOutput
  :: (MonadSafe mo, MonadCatch (Base mo))

  => NonPipe
  -- ^ Standard input

  -> NonPipe
  -- ^ Standard error

  -> CreateProcess

  -> IO (Producer ByteString mo ExitCode, ProcessHandle)
  -- ^ A 'Producer' for standard output
pipeOutput inp err cp = mask_ $ do
  pnl <- newProcessHandle (Just inp) Nothing (Just err) cp
  let pdcr = runOutputHandle Output pnl
  return (pdcr, pnl)


-- | Create a 'Producer' for standard error.
pipeError
  :: (MonadSafe me, MonadCatch (Base me))

  => NonPipe
  -- ^ Standard input

  -> NonPipe
  -- ^ Standard output

  -> CreateProcess

  -> IO (Producer ByteString me ExitCode, ProcessHandle)
  -- ^ A 'Producer' for standard error

pipeError inp out cp = mask_ $ do
  pnl <- newProcessHandle (Just inp) (Just out) Nothing cp
  let pdcr = runOutputHandle Error pnl
  return (pdcr, pnl)

-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard output.
pipeInputOutput
  :: ( MonadSafe mi, MonadCatch (Base mi),
       MonadSafe mo, MonadCatch (Base mo))

  => NonPipe
  -- ^ Standard error

  -> CreateProcess

  -> IO ( (Consumer ByteString mi ExitCode , Producer ByteString mo ExitCode)
        , ProcessHandle
        )
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output

pipeInputOutput err cp = mask_ $ do
  pnl <- newProcessHandle Nothing Nothing (Just err) cp
  let csmr = runInputHandle pnl
      pdcr = runOutputHandle Output pnl
  return ((csmr, pdcr), pnl)

-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard error.
pipeInputError
  :: ( MonadSafe mi, MonadCatch (Base mi),
       MonadSafe me, MonadCatch (Base me))

  => NonPipe

  -- ^ Standard output
  -> CreateProcess

  -> IO ( (Consumer ByteString mi ExitCode, Producer ByteString me ExitCode)
        , ProcessHandle
        )
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- error
pipeInputError out cp = do
  pnl <- newProcessHandle Nothing (Just out) Nothing cp
  let csmr = runInputHandle pnl
      pdcr = runOutputHandle Error pnl
  return ((csmr, pdcr), pnl)


-- | Create a 'Producer' for standard output and a 'Producer' for
-- standard error.
pipeOutputError
  :: ( MonadSafe mo, MonadCatch (Base mo),
       MonadSafe me, MonadCatch (Base me))

  => NonPipe
  -- ^ Standard input

  -> CreateProcess

  -> IO ( (Producer ByteString mo ExitCode, Producer ByteString me ExitCode)
        , ProcessHandle
        )
  -- ^ A 'Producer' for standard output and a 'Producer' for standard
  -- error

pipeOutputError inp cp = do
  pnl <- newProcessHandle (Just inp) Nothing Nothing cp
  let pdcrOut =  runOutputHandle Output pnl
      pdcrErr =  runOutputHandle Error pnl
  return ((pdcrOut, pdcrErr), pnl)


-- | Create a 'Consumer' for standard input, a 'Producer' for standard
-- output, and a 'Producer' for standard error.
pipeInputOutputError
  :: ( MonadSafe mi, MonadCatch (Base mi),
       MonadSafe mo, MonadCatch (Base mo),
       MonadSafe me, MonadCatch (Base me))

  => CreateProcess

  -> IO ( ( Consumer ByteString mi ExitCode
          , Producer ByteString mo ExitCode
          , Producer ByteString me ExitCode
          )
        , ProcessHandle
        )
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output, and a 'Producer' for standard error

pipeInputOutputError cp = do
  pnl <- newProcessHandle Nothing Nothing Nothing cp
  let csmr = runInputHandle pnl
      pdcrOut =  runOutputHandle Output pnl
      pdcrErr =  runOutputHandle Error pnl
  return ((csmr, pdcrOut, pdcrErr), pnl)
