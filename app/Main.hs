module Main where

import Lib
import Control.Monad

main :: IO ()
main = versosDelCanio >>= mapM_ putStrLn
