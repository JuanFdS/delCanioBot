module Lib where

import Test.QuickCheck (Arbitrary(..), sample', elements, Gen(..), discard, shuffle)
import Data.List (intercalate, nub)
import Control.Monad (mapM_)
import Data.Coerce (coerce)
import Control.Applicative ((<*>))

newtype DelCanio = DelCanio String deriving (Eq, Show, Ord)

rimasDelCanio :: [String]
rimasDelCanio = [
    "",
    "de caño",
    "de antaño",
    "aledaño",
    "fumandose un caño",
    "en la cola de un baño",
    "subiendo un peldaño",
    "metiendo un caño",
    "bailando en el caño",
    "el hitazo del año",
    "en un caño",
    "con caños",
    "hace daño"
    ]

generadorDeRimas :: Gen [String]
generadorDeRimas = take 4 <$> shuffle rimasDelCanio

versosDelCanio :: IO [String]
versosDelCanio = sample' $ ("Nico Del Caño " <>) . intercalate " " . filter (not.null) <$> generadorDeRimas
