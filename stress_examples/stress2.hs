import Control.Monad

main = e 76 ""
  where
    e i s = do {print s; when (i > 0) $  o (i-1) ('A':s)}
    o i s = do {print s; when (i > 0) $  e (i-1) ('B':s)}
