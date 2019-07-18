
module Main where

import           Canios

main :: IO ()
main = unaFrase <$> generadorCanio >>= putStrLn
    