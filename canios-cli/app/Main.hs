
module Main where

import           Canios

main :: IO ()
main = generadorCanio >>= putStrLn
    