-- | Examples using "Pipes.Proctee".  You will want to look at the
-- source code itself; viewing just the Haddocks will not help you
-- much.  You can view the source using Haddock if you used
-- @--hyperlink-source@ when building the library or if you are
-- viewing this on Hackage; look for the @Source@ link.  Or, you can
-- find the source at
--
-- <https://github.com/massysett/pipes-process/blob/master/lib/Pipes/Proctee/Examples.hs>

module Pipes.Proctee.Examples where

import Pipes
import Pipes.Proctee
import Pipes.Concurrent
import qualified Pipes.Proctee.Mailboxes as M
import Control.Monad.Trans.Cont
import qualified Data.ByteString.Char8 as BS8
import System.Exit
import System.Process (waitForProcess)

-- | Streams an infinite list of numbers to @less@.
numsToLess :: IO ExitCode
numsToLess = runProcess (useProxy prod id) inherit inherit
  (procSpec "less" [])
  where
    prod
      = each
      . fmap mkNumStr
      $ [(0 :: Int) ..]
    mkNumStr
      = flip BS8.snoc '\n'
      . BS8.pack
      . show

numsToLess' :: IO ExitCode
numsToLess' = flip runContT return $ do
  M.Wire (Just out) _ _ han <- M.createProcess
    (M.procSpec "less" []) { M.std_in = M.MakeMailbox }
  lift . runEffect $ prod >-> toOutput out
  lift $ waitForProcess han
  where
    prod
      = each
      . fmap mkNumStr
      $ [(0 :: Int) ..]
    mkNumStr
      = flip BS8.snoc '\n'
      . BS8.pack
      . show

