{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , UnliftedFFITypes
           , StandaloneDeriving
           , RankNTypes
  #-}


import GHC.Base
import Prelude

{-
  This exposes the setParent function. It also copies some stuff from
  Control.Concurrent that isnt exposed in stage-1. This is just for testing the
  call to the RTS.
-}


data ThreadId = ThreadId ThreadId#

-- | Returns the 'ThreadId' of the calling thread (GHC only).
myThreadId :: IO ThreadId
myThreadId = IO $ \s ->
   case (myThreadId# s) of (# s1, tid #) -> (# s1, ThreadId tid #)

setParent :: ThreadId -> IO ()
setParent (ThreadId tid) = IO $ \s ->
   case (setParent# tid s) of s1 -> (# s1, () #)

main :: IO ()
main = do
  tid <- myThreadId
  () <- setParent tid
  putStrLn "Hello world"
  return ()