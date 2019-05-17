module Util where

import Debug.Trace

mytrace :: String -> a -> a
mytrace = trace -- for debugging
-- mytrace str x = x