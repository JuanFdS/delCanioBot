module Lib(versosDelCanio) where

import           Test.QuickCheck                ( generate
                                                , Gen
                                                , shuffle
                                                , choose
                                                , frequency
                                                )

newtype DelCanio = DelCanio String deriving (Eq, Show, Ord)

rimasDelCanio :: [String]
rimasDelCanio =
    [ "de caño"
    , "de antaño"
    , "aledaño"
    , "fumandose un caño"
    , "en la cola de un baño"
    , "subiendo un peldaño"
    , "metiendo un caño"
    , "bailando en el caño"
    , "el hitazo del año"
    , "en un caño"
    , "con caños"
    , "hace daño"
    ]

generadorTamanio :: Gen Int
generadorTamanio = frequency [(10, choose (3, 4)), (1, return 5)]

generadorDeRimas :: Gen [String]
generadorDeRimas = do
    n <- generadorTamanio
    take n <$> shuffle rimasDelCanio

versosDelCanio :: IO String
versosDelCanio =
    generate
        $   ("Nico Del Caño " <>)
        .   unwords
        .   filter (not . null)
        <$> generadorDeRimas
    