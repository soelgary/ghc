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
      t1 <- busyWait 100000
      return 0
    _ -> return 0

analyze :: (String, Int) -> Int
analyze res =
  case res of
    ("A", _) -> 1
    _        -> 0

analyzePublicChannel :: LIORef DCLabel [String] -> DC Int
analyzePublicChannel publicChannel= do
  ref <- readLIORef publicChannel
  let acc = map (\x -> (head x, length x)) (group ref)
  let str = channelToStringHead acc
  () <- ioTCB $ print str
  return $ analyze $ head acc

runLowThreads :: DC Int
runLowThreads = do
  publicChannel <- newLIORef publicL []
  let cap1 = 1
  let cap2 = 2
  t1 <- lFork publicL (cap1Write len publicChannel)
  t2 <- lFork publicL (cap2Write len publicChannel)
  () <- lWait t1
  () <- lWait t2
  analyzePublicChannel publicChannel

mainDC :: DC (DCLabeled Int) -> DC Int
mainDC secret = do
  l <- getLabel
  let cap1 = 1
  let cap2 = 2
  t1 <- lFork publicL runLowThreads
  t2 <- lFork secretL (highThread secret)
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