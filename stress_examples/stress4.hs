
-- Run this with +RTS -M50000 and it should fail
-- Run this with +RTS -M800000 and it should pass
-- This tests that the parent gives up its blocks correctly

import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)

-- A simple function that prints three messages with a little delay between them.
printMessagesFrom name = for_ [1..3] printMessage
    where printMessage i = do
            sleepMs 1
            putStrLn (name ++ " number " ++ show i)

-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)

main = do
    -- Fork a new thread to do some work in the background.
    forkIO (printMessagesFrom "fork1")
    forkIO (printMessagesFrom "fork2")
    forkIO (printMessagesFrom "fork3")
    forkIO (printMessagesFrom "fork4")

    -- Wait for threads to finish.
    sleepMs 10