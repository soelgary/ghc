module Network.Wai.Handler.Warp.Handler where

initialTickSize :: Int
initialTickSize = 200

minTickSize :: Int
minTickSize = 5

forkTickSize :: Int
forkTickSize = 5

{-
  The main core is going to dispatch requests
  Every other core will run a handler
    -the handler is the main hthread
    -it will spawn threads for each request and manage CPU time

-}