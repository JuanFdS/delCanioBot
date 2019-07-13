
module Main where

import           Lib

main :: IO ()
main = generadorCanio >>= putStrLn
    