module Debug (debug, debugF) where

import Debug.Trace (traceShow)

debug :: Show b => b -> b
debug a = traceShow a a

debugF :: Show c => (a -> c) -> a -> c
debugF f = (\a -> traceShow a a) . f
