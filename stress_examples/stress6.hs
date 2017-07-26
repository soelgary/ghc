{-# LANGUAGE ScopedTypeVariables #-}

-- Run this with +RTS -M50000 and it should fail
-- Run this with +RTS -M800000 and it should pass
-- This tests that the parent gives up its blocks correctly

import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)
import Data.IORef
import Debug.Trace

--import qualified Data.Array.IO as Array
import qualified Control.Exception as Exception

import Data.List 
import Control.Concurrent
import Control.Monad
import System.IO
import Prelude hiding (max,min)

-- A simple function that prints three messages with a little delay between them.
printMessagesFrom name = for_ [1..3] printMessage
    where printMessage i = do
            sleepMs 1
            putStrLn (name ++ " number " ++ show i)

-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)

len :: Int
len = 10

max :: Int
max = 10000

min :: Int
min = 0

type Msg = String 
type Chann = IORef [Msg]

windowSize = 200000
msgCount = 100000

message :: Int -> Msg
message = concat . replicate 1024 . show 

pushMsg :: Chann -> Int -> IO ()
pushMsg chan =
    modifyIORef chan . (:) <=< Exception.evaluate . message 

customTrace :: IORef [String] -> String -> IO ()
customTrace ref = modifyIORef' ref . (:)

bump mv = putMVar mv . (+1) =<< takeMVar mv


--first :: IORef [String] -> Chann -> IO ()
first mv ref root =  replicateM_ (len *2) (b >> a >> bump mv)
  where a =  customTrace ref "HHHHH"
        b = mapM_ (pushMsg root) [0..min] 
  
--second :: IORef [String] -> Chann -> IO ()
second mv ref root = const (bump mv)  =<< const (replicateM_ len a) =<< replicateM_ 500 (b >> a)
  where a = customTrace ref "LLLLLL"
        b = mapM_ (pushMsg root) [0..1]

--third :: IORef [String] -> IO ()
third mv ref = replicateM_ len a >> bump mv
  where a = customTrace ref "GGGGGG"
  
mainDC :: Chann -> IORef [String] -> IO Int
mainDC root ref = do
--  l <- getLabel
  mv <- newMVar 0
  t1 <- forkIO (first mv ref root)
  --() <- lWait t1
  t2 <- forkIO (second mv ref root)
  t3 <- forkIO (third mv ref)
  takeMVar mv >>= print
--  () <- lWait t2
--  () <- lWait t3

  return 1

chunksof n [] = []
chunksof n xs | n < length xs = [xs]
              | otherwise = uncurry (:) . fmap (chunksof n) $ splitAt n xs

main = do
    -- Fork a new thread to do some work in the background.
    forkIO (printMessagesFrom "fork1")
    forkIO (printMessagesFrom "fork2")
    forkIO (printMessagesFrom "fork3")
    forkIO (printMessagesFrom "fork4")

    -- Wait for threads to finish.
    sleepMs 1000
    ref <- newIORef []
    root <- newIORef []
    res <- mainDC root ref
  --  res <- evalDC (mainDC root ref)
    readIORef ref >>= print
                    . filter ((/= 'H') .head .head)
                    -- . map (\x -> (head x,length x))
                    -- . map pure
                    . chunksof 500
                    -- . group
    readIORef ref >>= print . length
    return ()
