
module Main where

import           Twitter
import           Canios

main :: IO ()
main = generadorCanio >>= post
    