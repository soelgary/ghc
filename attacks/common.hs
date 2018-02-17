{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns
           , MagicHash
           , UnboxedTuples
           , UnliftedFFITypes
           , StandaloneDeriving
           , RankNTypes
  #-}


module Common (
  getArg
, publicL
, secretL
, initialLabelState
, channelToStringHead
, channelToStringLast
, customTrace
, cap1Write
, cap2Write
, busyWait
, busyWait'
, channelToStringRec
, hFork
, hKill
, hKill'
  ) where





import LIO hiding (catch)
import LIO.LIORef
import LIO.DCLabel
import LIO.Concurrent
import LIO.Concurrent.LMVar
import LIO.Core
import LIO.Exception
import LIO.Error
import LIO.Label
import LIO.TCB

import Control.Monad

import Data.List

import qualified Control.Concurrent as IO
import qualified Control.Exception as IO
import Control.Monad
import Data.IORef





--- START LIOPAR ---

hFork :: Label l
      => Int
      -> l                -- ^ Label of result
      -> LIO l a          -- ^ Computation to execute in separate thread
      -> LIO l (LabeledResult l a) -- ^ Labeled result
hFork cap = lForkPC cap noPrivs

-- | Same as 'lFork', but the supplied set of priviliges are accounted
-- for when performing label comparisons.
lForkPC :: PrivDesc l p =>
          Int -> Priv p -> l -> LIO l a -> LIO l (LabeledResult l a)
lForkPC cap p l (LIOTCB action) = do
  withContext "lForkP" $ guardAllocP p l
  mv <- ioTCB IO.newEmptyMVar
  st <- ioTCB $ newIORef LResEmpty
  s0 <- getLIOStateTCB
  () <- ioTCB $ IO.releaseTime 4
  tid <- ioTCB $ IO.mask $ \unmask -> IO.hForkOn cap 4 $ do
    sp <- newIORef s0
    ea <- IO.try $ unmask $ action sp
    LIOState lEnd _ <- readIORef sp
    writeIORef st $ case ea of
      _ | not (lEnd `canFlowTo` l) -> LResLabelTooHigh lEnd
      Left e                       -> LResResult $ IO.throw $ makeCatchable e
      Right a                      -> LResResult a
    IO.putMVar mv ()
  return $ LabeledResultTCB tid l mv st

{-|
Kills the thread after the specified amount of time.
TODO: Check labels can flow according to semantics.
TODO: Handle exceptions
TODO: Reclaim resources accordingly
-}
hKill :: Label l => LabeledResult l a -> Int -> Int -> String -> LIO l ()
hKill (LabeledResultTCB tid _ _ _) microseconds cap msg = do
  () <- ioTCB $ IO.threadDelay microseconds
  () <- ioTCB $ print $ "Killing...." ++ msg
  () <- ioTCB $ IO.forceOnQueue tid
  --() <- ioTCB $ print "Done waiting...."
  ticks <- ioTCB $ IO.hKillThread tid
  --() <- ioTCB $ print "Killed...."
  () <- ioTCB $ IO.addTime ticks
  --() <- ioTCB $ print "Ticks reclaimed...."
  return ()

hKill' :: Label l => LabeledResult l a -> Int -> Int -> LIO l ()
hKill' (LabeledResultTCB tid _ _ _) microseconds cap = do
  () <- ioTCB $ IO.threadDelay microseconds
  () <- ioTCB $ print "Killing....!!!"
  --() <- ioTCB $ print "Done waiting...."
  ticks <- ioTCB $ IO.hKillThread tid
  --() <- ioTCB $ print "Killed...."
  return ()

--- END LIOPAR ---


-- Command line parsing
getArg [] = -1
getArg (a:r) = read a
-- End command line parsing

-- Labels
publicL = "Public" %% "Public"
l3 = "Secret" %% "Secret"
secretL = publicL `lub` l3
initialLabelState = LIOState { lioLabel     = publicL
                              , lioClearance = secretL }
-- End Labels

-- Extracts the first group of the channel and converts it to a string
-- This is useful for building the analyzers
channelToStringLast :: [(String,Int)] -> (String,(String,Int))
channelToStringLast [] = ("",("",0))
channelToStringLast l =
  let (s',c') = last l in
  let str = "(" ++ s' ++ "," ++ " " ++ (show c') ++ ")" in
  (str,(s',c'))

channelToStringHead :: [(String,Int)] -> (String,(String,Int))
channelToStringHead [] = ("",("",0))
channelToStringHead l =
  let (s',c') = head l in
  let str = "(" ++ s' ++ "," ++ " " ++ (show c') ++ ")" in
  (str,(s',c'))

channelToStringRec :: [(String,Int)] -> String
channelToStringRec c =
  let app acc (s,c) = acc ++ ", " ++ "(" ++ s ++ "," ++ " " ++ (show c) ++ ")" in
    foldl' app "" c

-- Utilities for writing to public channels
customTrace :: LIORef DCLabel [String] -> String -> DC ()
customTrace ref s = do
  () <- modifyLIORef ref (\l -> s:l)
  return ()

write :: String -> Int -> LIORef DCLabel [String] -> DC ()
write c len ref = replicateM_ len trace
  where trace = do
          () <- modifyLIORef ref (\l -> c:l)
          return ()

cap1Write :: Int -> LIORef DCLabel [String] -> DC ()
cap1Write len ref = write "A" len ref

cap2Write :: Int -> LIORef DCLabel [String] -> DC ()
cap2Write len ref = write "B" len ref

cap3Write :: Int -> LIORef DCLabel [String] -> DC ()
cap3Write len ref = write "C" (len * 3) ref
-- End utilities for writing to public channels

-- Utilities for waiting
busyWait :: Int -> DC Int
busyWait 0 = return 1
busyWait n = do
  acc <- busyWait (n - 1)
  return $ acc + n

-- Like busyWait, but it writes to a secret channel lots of times instead of
-- dummy recursing
busyWait' :: Int -> DC Int
busyWait' n = do
  secretChannel <- newLIORef secretL []
  () <- cap3Write n secretChannel
  return 1
-- End utilities for waiting