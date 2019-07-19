{-# LANGUAGE FlexibleContexts, DerivingVia, StandaloneDeriving, NamedFieldPuns, FlexibleInstances, FlexibleContexts #-}
module Imagen where

import           Graphics.Image hiding (minimum, maximum, map, on)
import           Graphics.Image.Interface hiding (map)
import           qualified Graphics.Image as G hiding (minimum, maximum)
import           Data.Functor.Compose
import           Data.List
import           Data.List.Index
import           Data.Function (on)
import Debug.Trace

type Imagen = Image VS RGBA Double

invisible :: AlphaSpace cs e => Pixel cs e -> Bool
invisible = (==0) . getAlpha

visible :: AlphaSpace cs e => Pixel cs e -> Bool
visible = not . invisible

overlapImage :: Imagen -> Imagen -> Imagen
overlapImage = G.zipWith (\firstPixel secondPixel -> if invisible firstPixel then secondPixel else firstPixel)

adaptCanvasSizes :: Imagen -> Imagen -> (Imagen, Imagen)
adaptCanvasSizes anImage anotherImage = (canvasSize (Fill 0) maxSize anImage, canvasSize (Fill 0) maxSize anotherImage)
    where maxSize = maxCanvasSize anImage anotherImage

maxCanvasSize :: Imagen -> Imagen -> Dimension
maxCanvasSize anImage anotherImage = ((max `on` rows) anImage anotherImage, (max `on` cols) anImage anotherImage)

adaptCanvasSizeTo :: Imagen -> Imagen -> Imagen
adaptCanvasSizeTo anImage anotherImage = fst (adaptCanvasSizes anImage anotherImage)

instance Num a => Num (a, a) where
    (a, b) + (c, d) = (a + c, b + d)
    (a, b) * (c, d) = (a * c, b * d)
    (a, b) - (c, d) = (a - c, b - d)
    abs (a, b) = (abs a, abs b)
    fromInteger x = (fromInteger x, fromInteger x)
    signum (a, b) = (signum a, signum b)

instance Enum a => Enum (a, a)

instance Real a => Real (a, a)

instance Integral a => Integral (a, a) where
    (a,b) `quot` (c,d) = (a `quot` c, b `quot` d)
    (a,b) `div` (c, d) = (a `div` c, b `div` d)

boundingBox :: Imagen -> BoundingBox
boundingBox image = BoundingBox { top = minY, bottom = maxY, right = maxX, left = minX }
                where visiblePixelDimensions = map (\(dimensions, pixel) -> dimensions) $ filter (\(_, pixel) -> not . invisible $ pixel) $ concat . matrixToLists $ indexedMap (\dimensions pixel -> (dimensions, pixel)) (Matrix $ toLists image)
                      minX = minimum $ fmap fst $ visiblePixelDimensions
                      minY = minimum $ fmap snd $ visiblePixelDimensions
                      maxX = maximum $ fmap fst $ visiblePixelDimensions
                      maxY = maximum $ fmap snd $ visiblePixelDimensions

newtype Matrix a = Matrix [[a]] deriving (Functor, Foldable) via (Compose [] [])

matrixToLists :: Matrix a -> [[a]]
matrixToLists (Matrix lists) = lists

indexedMap :: ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
indexedMap f (Matrix matrix) = Matrix $ (\(x, value) -> (\(y, value) -> f (x, y) value) <$> indexed value) <$> indexed matrix

-- Bounding Box

data BoundingBox = BoundingBox { left :: Int, bottom :: Int, top :: Int, right :: Int } deriving (Eq, Show)

origin :: BoundingBox -> Dimension
origin boundingBox = (top boundingBox, left boundingBox)

dimensions :: BoundingBox -> Dimension
dimensions boundingBox = (width boundingBox, height boundingBox)

width :: BoundingBox -> Int
width boundingBox = abs $ right boundingBox - left boundingBox

height :: BoundingBox -> Int
height boundingBox = abs $ top boundingBox - bottom boundingBox

isEdge :: Int -> Dimension -> Dimension -> Bool
isEdge tolerance (pixelY, pixelX) (rows, cols) =
    pixelY <= tolerance || pixelX <= tolerance || pixelY >= rows - tolerance || pixelX >= cols - tolerance

drawSquareFromBB :: BoundingBox -> Imagen
drawSquareFromBB boundingBox =
    makeImage (dimensions boundingBox) $ \pixelPosition ->
                                            if isEdge 5 pixelPosition (dimensions boundingBox)
                                                then redPixel
                                                else emptyPixel
    where emptyPixel = promote 0
          redPixel = fromComponents (1, 0, 0, 1)


-- Transformaciones considerando el Bounding Box

data ImagenDelCanio = ImagenDelCanio { imagen :: Imagen, boundingBoxDelCanio :: BoundingBox } deriving Show

data Where = OnTop | Behind

data DireccionVertical = Arriba | Abajo
data DireccionHorizontal = Izquierda | Derecha
type DireccionDiagonal = (DireccionVertical, DireccionHorizontal)

type Dimension = (Int, Int)

mostrar :: ImagenDelCanio -> IO ()
mostrar = displayImage . imagen 

mostrarConBB :: ImagenDelCanio -> IO ()
mostrarConBB = mostrar . delCanioWithBB

translateRespectToBB :: DireccionDiagonal -> ImagenDelCanio -> Dimension -> Imagen -> Imagen
translateRespectToBB direccion nico (rows, cols) anotherImage =
    translateIncreasingCanvas direccion (rows + (top $ boundingBoxDelCanio nico), cols + (left $ boundingBoxDelCanio nico)) anotherImage

scaleRespectToBB :: ImagenDelCanio -> (Double, Double) -> Imagen -> Imagen
scaleRespectToBB = scaleRespectToDimensions . dimensions . boundingBoxDelCanio

scaleRespectToTotalImage :: ImagenDelCanio -> (Double, Double) -> Imagen -> Imagen
scaleRespectToTotalImage = scaleRespectToDimensions . dims . imagen

scaleRespectToDimensions :: Dimension -> (Double, Double) -> Imagen -> Imagen
scaleRespectToDimensions dimensions proportions =
    scale Bilinear Edge proportions . resize Bilinear Edge dimensions

deAntanio :: ImagenDelCanio -> IO ImagenDelCanio
deAntanio (ImagenDelCanio imagenDelCanio bb) = return $ ImagenDelCanio (G.map sepiaPixel imagenDelCanio) bb

sepiaPixel :: Pixel RGBA Double -> Pixel RGBA Double
sepiaPixel unPixel = fromComponents (rojoSepia, verdeSepia, azulSepia, alphaOriginal)
    where rojoOriginal = getPxC unPixel RedRGBA
          verdeOriginal = getPxC unPixel GreenRGBA
          azulOriginal = getPxC unPixel BlueRGBA
          rojoSepia = min 1 ((0.393 * rojoOriginal + 0.769 * verdeOriginal + 0.189 * azulOriginal) * 1.1)
          verdeSepia = min 1 ((0.349 * rojoOriginal + 0.686 * verdeOriginal + 0.168 * azulOriginal) * 1.1)
          azulSepia = min 1 ((0.272 * rojoOriginal + 0.534 * verdeOriginal + 0.131 * azulOriginal) * 0.9)
          alphaOriginal = getPxC unPixel AlphaRGBA

horizontalOpposite :: DireccionHorizontal -> DireccionHorizontal
horizontalOpposite Izquierda = Derecha
horizontalOpposite Derecha = Izquierda

verticalOpposite :: DireccionVertical -> DireccionVertical
verticalOpposite Arriba = Abajo
verticalOpposite Abajo = Arriba

opposite :: DireccionDiagonal -> DireccionDiagonal
opposite (vertical, horizontal) = (verticalOpposite vertical, horizontalOpposite horizontal)

capped :: Dimension -> Dimension -> Dimension
capped (maxRows, maxColumns) (rows, columns) = (rows `max` maxRows, columns `max` maxColumns)

canvasIncreasementTranslation :: DireccionDiagonal -> Dimension -> Dimension
canvasIncreasementTranslation direccion = capped (0, 0)  . translation (opposite direccion)

translation :: DireccionDiagonal -> Dimension -> Dimension
translation (direccionVertical, direccionHorizontal) (rows, columns) = (translateY direccionVertical, translateX direccionHorizontal)
            where translateX Izquierda = negate columns
                  translateX Derecha = columns
                  translateY Arriba = negate rows
                  translateY Abajo = rows

translateIncreasingCanvas :: DireccionDiagonal -> Dimension -> Imagen -> Imagen
translateIncreasingCanvas direccion deltaPosicion =
    translate' direccion deltaPosicion . increaseCanvasSize direccion deltaPosicion

translateBoundingBox' :: Dimension -> BoundingBox -> BoundingBox
translateBoundingBox' (deltaRows, deltaColumns) (BoundingBox {top, bottom, left, right}) = BoundingBox {
    top = top + deltaRows,
    bottom = bottom + deltaRows,
    left = left + deltaColumns,
    right = right + deltaColumns
}

increaseCanvasSize :: DireccionDiagonal -> Dimension -> Imagen -> Imagen
increaseCanvasSize direccion (deltaRows, deltaColumns) imagen = translate (Fill 0) translation $ canvasSize (Fill 0) newDimensions imagen
        where newDimensions = (rows imagen + abs deltaRows, cols imagen + abs deltaColumns)
              translation = canvasIncreasementTranslation direccion (deltaRows, deltaColumns)

increaseCanvasSizeForBoundingBox :: DireccionDiagonal -> Dimension -> BoundingBox -> BoundingBox
increaseCanvasSizeForBoundingBox direccion deltaPosicion = translateBoundingBox' (canvasIncreasementTranslation direccion deltaPosicion)

increaseCanvasSizeWithBB :: DireccionDiagonal -> Dimension -> ImagenDelCanio -> ImagenDelCanio
increaseCanvasSizeWithBB direccion deltaPosicion (ImagenDelCanio imagen bb) =
    ImagenDelCanio (increaseCanvasSize direccion deltaPosicion imagen) (increaseCanvasSizeForBoundingBox direccion deltaPosicion bb)

data OnEdge = IncreaseCanvas | Crop

translateWithBB :: OnEdge -> DireccionDiagonal -> Dimension -> ImagenDelCanio -> ImagenDelCanio
translateWithBB onEdge direccion deltaPosicion (ImagenDelCanio imagen bb) =
    ImagenDelCanio (translateImage direccion deltaPosicion imagen) (translateBoundingBox direccion deltaPosicion bb)
    where translateImage = case onEdge of
            IncreaseCanvas -> translateIncreasingCanvas
            Crop -> translate'

translate' :: DireccionDiagonal -> Dimension -> Imagen -> Imagen
translate' direccion deltaPosicion = translate (Fill 0) (translation direccion deltaPosicion)

translateBoundingBox :: DireccionDiagonal -> Dimension -> BoundingBox -> BoundingBox
translateBoundingBox direccion deltaPosicion = translateBoundingBox' (translation direccion deltaPosicion)

centerBoundedArea :: ImagenDelCanio -> ImagenDelCanio
centerBoundedArea imagenDelCanio@(ImagenDelCanio imagen bb) = translateWithBB Crop (Abajo, Derecha) dimensionsUntilCenter imagenDelCanio
        where dimensionsUntilCenter = (dims imagen - (height bb, width bb)) `div` 2 - (left bb, top bb)

adaptCanvasSizes' :: ImagenDelCanio -> Imagen -> (ImagenDelCanio, Imagen)
adaptCanvasSizes' delCanio@(ImagenDelCanio imagen _) anotherImage =
    (increaseCanvasSizeWithBB (Abajo, Derecha) (deltaDimensions imagen) delCanio,
     increaseCanvasSize (Abajo, Derecha) (deltaDimensions anotherImage) anotherImage)
        where deltaDimensions img = capped (0, 0) (maxCanvasSize imagen anotherImage - dims img)

delCanioWithBB :: ImagenDelCanio -> ImagenDelCanio
delCanioWithBB delCanio@(ImagenDelCanio imagen bb) = ImagenDelCanio (overlapImage bbImageUbicada imagen) bb
    where bbImage = drawSquareFromBB bb
          (_, bbImageAdaptada) = adaptCanvasSizes' delCanio bbImage
          bbImageUbicada = translate' (Abajo, Derecha) (origin bb) bbImageAdaptada

mergeWithBB :: Where -> Imagen -> ImagenDelCanio -> ImagenDelCanio
mergeWithBB donde anotherImage nico = ImagenDelCanio (overlap anotherImageAdaptada imagenAdaptada) bb
    where (ImagenDelCanio imagenAdaptada bb, anotherImageAdaptada) = adaptCanvasSizes' nico anotherImage
          overlap = case donde of
            OnTop -> overlapImage
            Behind -> flip overlapImage

-- Transformaciones sobre la imagen de del canio

delCanio :: IO ImagenDelCanio
delCanio = do
    imagenDelCanio <- readImageRGBA VS "imagenes/delCanio.png"
    pure $ ImagenDelCanio imagenDelCanio (boundingBox imagenDelCanio)

conCanio :: ImagenDelCanio -> IO ImagenDelCanio
conCanio nico = do
    arma <- readImageRGBA VS "imagenes/arma.png"
    let armaEscalada = scaleRespectToBB nico (0.5, 0.5) arma
        armaTransladada = translateRespectToBB (Abajo, Derecha) nico (150, 150) armaEscalada
    pure $ mergeWithBB OnTop armaTransladada nico

fumandoseUnCanio :: ImagenDelCanio -> IO ImagenDelCanio
fumandoseUnCanio nico = do
    porro <- readImageRGBA VS "imagenes/fumandoseUnCanio.png"
    let porroEscalado = scaleRespectToBB nico (0.3, 0.3) porro
        porroTransladado = translateRespectToBB (Abajo, Derecha) nico (115, 140) porroEscalado
    pure $ mergeWithBB OnTop porroTransladado nico

enUnCanio :: ImagenDelCanio -> IO ImagenDelCanio
enUnCanio nico = do
    tubo <- readImageRGBA VS "imagenes/enUnCanio.jpg"
    let tuboEscalado = scaleRespectToTotalImage nico (1.5, 1.5) tubo
        nicoEscalado = centerBoundedArea . fst . adaptCanvasSizes' nico $ tuboEscalado
    pure $ mergeWithBB Behind tuboEscalado nicoEscalado

bailandoEnElCanio :: ImagenDelCanio -> IO ImagenDelCanio
bailandoEnElCanio nico = do
    canio <- readImageRGBA VS "imagenes/bailandoEnElCanio.png"
    let canioEscalado = scaleRespectToBB nico (5, 2) canio
        (nicoAdaptado, canioAdaptado) = adaptCanvasSizes' nico $ canioEscalado
        nicoUbicado = translateWithBB IncreaseCanvas (Abajo, Derecha) (100, 225) nicoAdaptado
    pure $ mergeWithBB Behind canioEscalado nicoUbicado

aledanio :: ImagenDelCanio -> IO ImagenDelCanio
aledanio nico = do
    boina <- readImageRGBA VS "imagenes/boina.png"
    termo <- readImageRGBA VS "imagenes/termo.png"
    let boinaEscalada = scaleRespectToBB nico (0.4, 0.7) boina
        termoEscalado = scaleRespectToBB nico (0.85, 0.45) termo
        boinaTransladada = translateRespectToBB (Abajo, Derecha) nico (0, 60) boinaEscalada
        termoTransladado = translateRespectToBB (Abajo, Derecha) nico (115, 0) termoEscalado
    pure . mergeWithBB OnTop termoTransladado . mergeWithBB OnTop boinaTransladada $ nico
