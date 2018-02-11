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
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Hfork
(
  hForkOn
) where

import Foreign
import Foreign.C

import Data.Typeable
import Data.Maybe

import GHC.Base

import GHC.IO
import GHC.IO.Encoding.UTF8
import GHC.IO.Exception
import GHC.Exception
import qualified GHC.Foreign
import GHC.IORef
import GHC.MVar
import GHC.Ptr
import GHC.Real         ( fromIntegral )
import GHC.Show         ( Show(..), showString )
import GHC.Stable       ( StablePtr(..) )
import GHC.Weak

import Data.Either

import Control.Concurrent (ThreadId(..))


hForkOn :: Int -> Int -> IO () -> IO ThreadId
hForkOn (I# cpu) (I# ticks) action = IO $ \ s ->
   case (hFork# cpu ticks action_plus s) of (# s1, tid #) -> (# s1, ThreadId tid #)
 where
  -- We must use 'catch' rather than 'catchException' because the action
  -- could be bottom. #13330
  action_plus = catch action childHandler