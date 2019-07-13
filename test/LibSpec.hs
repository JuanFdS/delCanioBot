
module LibSpec where

import           Test.Hspec
import           Test.QuickCheck                ( property )
import           Data.List

import           Lib

spec :: Spec
spec = do
    describe "generador de frases" $ do
        it "siempre empiezan igual" $ property $ 
            generadorCanio >>= (`shouldSatisfy` isPrefixOf "Nico Del Ca\241o ")
