
module CaniosSpec where

import           Test.Hspec
import           Test.QuickCheck                ( property )
import           Data.List

import           Canios

spec :: Spec
spec = do
    describe "generador de frases" $ do
        it "siempre empiezan igual" $ property $ 
            unaFrase <$> generadorCanio >>= (`shouldSatisfy` isPrefixOf "Nico Del Caño ")
