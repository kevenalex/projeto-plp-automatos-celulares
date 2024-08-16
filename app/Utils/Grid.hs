module Utils.Grid (gridGenerate) where

import qualified Utils.Terminal
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Matrix


gridGenerate :: Int -> Int -> a -> Matrix a
gridGenerate width height a = matrix width height (\_ -> a)