{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module HBLAS.Level3 where

import qualified Data.Vector as V
import qualified Data.Matrix as M
import HBLAS.Level1
import HBLAS.Level2
import Prelude hiding (zipWith,(!!), foldl')
import Data.Monoid
