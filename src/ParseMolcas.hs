{-# Language FlexibleContexts,BangPatterns #-}

module ParseMolcas where


import qualified Data.Vector.Unboxed as VU
import Data.Array.Repa as R
import Data.Array.Repa.Unsafe as R
import Control.Applicative
import Control.Arrow ((&&&))