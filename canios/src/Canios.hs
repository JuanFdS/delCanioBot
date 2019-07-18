
module Canios(generadorCanio, unaImagen, unaFrase, verso, fImagen, Canio) where

import           Test.QuickCheck                ( generate
                                                , Gen
                                                , vectorOf
                                                , choose
                                                , frequency
                                                , variant
                                                , shuffle
                                                )
import qualified  Data.ByteString.Lazy.Internal as BS
import            Imagen
import qualified Graphics.Image as I
import qualified Graphics.Image.IO.Formats as I

data Canio = Canio {
                        verso :: String,
                        fImagen :: ImagenDelCanio -> IO ImagenDelCanio
                    }
                    
posibilidades :: [Canio]
posibilidades =  [ Canio "de caño" return
                 , Canio "de antaño" deAntanio
                 , Canio "aledaño" return
                 , Canio "fumandose un caño" fumandoseUnCanio
                 , Canio "en la cola de un baño" return
                 , Canio "subiendo un peldaño" return
                 , Canio "metiendo un caño" enUnCanio
                 , Canio "bailando en el caño" return
                 , Canio "el hitazo del año" return
                 , Canio "en un caño" enUnCanio
                 , Canio "hace daño" return
                 , Canio "con un caño" conCanio
                ]

generadorTamanio :: Gen Int
generadorTamanio = frequency [(10, choose (3, 4)), (1, return 5)]

generadorDeCanios :: Gen [Canio]
generadorDeCanios = do
    n <- generadorTamanio
    take n <$> shuffle posibilidades

generadorCanio :: IO [Canio]
generadorCanio =
    generate generadorDeCanios

imagenInicial = delCanio
versoInicial = "Nico Del Caño "

transformarABS :: ImagenDelCanio -> BS.ByteString
transformarABS = I.encode I.PNG [] . imagen

unaImagen :: [Canio] -> IO BS.ByteString
unaImagen canios = 
    transformarABS <$> foldl (>>=) imagenInicial (map fImagen canios)
    
 
unaFrase :: [Canio] -> String
unaFrase canios =
    (versoInicial <>) $ unwords $ map verso canios