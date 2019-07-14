{-# LANGUAGE FlexibleContexts, DerivingVia, StandaloneDeriving #-}
module Imagen where

import           Graphics.Image hiding (minimum, maximum, map)
import           Graphics.Image.Interface hiding (map)
import           qualified Graphics.Image as G hiding (minimum, maximum)
import           Data.Functor.Compose
import           Data.List
import           Data.List.Index

type Imagen = Image VU RGBA Double

invisible :: AlphaSpace cs e => Pixel cs e -> Bool
invisible = (==0) . getAlpha

delCaniopng = readImageRGBA VU "/home/juan/Desktop/moton306.png"
windowsXpWallpaper = readImageRGBA VU "/home/juan/Desktop/xpbliss_7.jpg"

delCanioadaptadopng = do
    delCanio <- delCaniopng
    windowsXP <- windowsXpWallpaper
    let (delCanioAdaptado, _) = adaptSizeCentering delCanio windowsXP
    return delCanioAdaptado

overlapImage :: (Array arr cs e, Graphics.Image.Interface.AlphaSpace cs e) =>
                    Image arr cs e -> Image arr cs e -> Image arr cs e
overlapImage = G.zipWith (\firstPixel secondPixel -> if invisible firstPixel then secondPixel else firstPixel)

adaptCanvasSizes :: (Array arr1 cs1 e1, Array arr cs e) =>
    Image arr cs e -> Image arr1 cs1 e1 -> (Image arr cs e, Image arr1 cs1 e1)
adaptCanvasSizes anImage anotherImage =
    (canvasSize (Fill 0) maxSize anImage, canvasSize (Fill 0) maxSize anotherImage)
    where maxSize = (max (rows anImage) (rows anotherImage), max (cols anImage) (cols anotherImage))

adaptSizeCentering anImage anotherImage = (centeredImage, centeredAnotherImage)
    where centeredImage = center adaptedImage (dims anImage)
          centeredAnotherImage = center adaptedAnotherImage (dims anotherImage)
          center imagen (originalY, originalX) = translate (Fill 0) (rows imagen `div` 2 - originalY `div` 2, cols imagen `div` 2 - originalX `div` 2) imagen
          (adaptedImage, adaptedAnotherImage) = adaptCanvasSizes anImage anotherImage

-- translateRandomly (x, y) image = 

mostrarDelCanioCentrado = do
    delCanio <- delCaniopng
    windowsXP <- windowsXpWallpaper
    let (delCanioCentrado, _) = adaptSizeCentering delCanio windowsXP
    displayImage delCanioCentrado

mergeImages anImage anotherImage = uncurry overlapImage $ adaptSizeCentering anImage anotherImage

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

data BoundingBox = BoundingBox { left :: Int, bottom :: Int, top :: Int, right :: Int } deriving (Eq, Show)

width boundingBox = abs $ right boundingBox - left boundingBox
height boundingBox = abs $ top boundingBox - bottom boundingBox

leftTop boundingBox = (left boundingBox, top boundingBox)
rightBottom boundingBox = (right boundingBox, bottom boundingBox)

isContained bb (x, y) = right bb <= x && bottom bb <= y

deriving instance (Eq a) => Eq (Matrix a)

instance (Show a) => Show (Matrix a) where
    show (Matrix lists) = intercalate "\n" $ Prelude.map (intercalate "|" . Prelude.map show) lists

-- TODO: Revisar esto con menos
drawBoundingBox :: BoundingBox -> Imagen
drawBoundingBox boundingBox = G.map (\pixel -> if invisible pixel then fromComponents (1,0,0,1) else promote 0) $ G.zipWith const rellenoAdaptado contornoAdaptado
    where contorno = makeImage (width boundingBox, height boundingBox) (const $ promote 1) :: Imagen
          relleno = makeImage (width boundingBox - 10, height boundingBox - 10) (const $ promote 1) :: Imagen
          (contornoAdaptado, rellenoAdaptado) = adaptSizeCentering contorno relleno

taparConBB imagen = mergeImages imagen wbbAdaptadoUbicado
    where bb = boundingBox imagen
          wbb = drawBoundingBox bb
          (imagenAdaptada, wbbadaptado) = adaptCanvasSizes imagen wbb
          wbbAdaptadoUbicado = translate (Fill 0) (leftTop bb) wbbadaptado

data Where = OnTop | Behind

decorarDelCanio :: (Int, Int) -> (Double, Double) -> Where -> Imagen -> ImagenDelCanio -> ImagenDelCanio
decorarDelCanio posicionRelativa tamanioRelativo donde otraImagen (ImagenDelCanio imagenDelCanio bbDelCanio) =
    ImagenDelCanio imagenFinal nuevoBB

        where otraImagenAdaptada = translate (Fill 0) posicionRelativa .
                                increaseCanvasSize posicionRelativa .
                                resize Bilinear Edge tamanio .
                                fst .
                                adaptCanvasSizes otraImagen $ imagenDelCanio

              tamanio = (round $ fromIntegral (rows imagenDelCanio) * fst tamanioRelativo,
                         round $ fromIntegral (cols imagenDelCanio) * snd tamanioRelativo)

              imagenFinal = cropByBoundingBox $ superponer donde otraImagenAdaptada imagenDelCanio
              (xFinal, yFinal) = dims imagenFinal
              (xInicial, yInicial) = dims imagenDelCanio
              deltaWidth = xFinal - xInicial
              deltaHeight = yFinal - yInicial
              nuevoBB = bbDelCanio { left = left bbDelCanio - deltaWidth `div` 2, right = right bbDelCanio - deltaWidth `div` 2,
                                     top = top bbDelCanio - deltaHeight `div` 2, bottom = top bbDelCanio - deltaHeight `div` 2 }

actualizarBB (ImagenDelCanio imagenDelCanio bbDelCanio) = ImagenDelCanio imagenDelCanio (bbDelCanio {
        bottom = (fst . dims $ imagenDelCanio), top = 0, right = (snd . dims $ imagenDelCanio), left = 0
    })

superponer OnTop = mergeImages
superponer Behind = flip mergeImages

cropByBoundingBox imagen = crop (leftTop bb) (width bb, height bb) imagen
    where bb = boundingBox imagen

increaseCanvasSize (x, y) image = canvasSize (Fill 0) (x + cols image, y + rows image) image

delCanio :: IO ImagenDelCanio
delCanio = (\imagenDelCanio -> ImagenDelCanio imagenDelCanio (boundingBox imagenDelCanio)) <$> delCaniopng

conCanio :: ImagenDelCanio -> IO ImagenDelCanio
conCanio nico = (\arma -> decorarDelCanio (200, 250) (0.5, 0.5) OnTop arma nico) <$> readImageRGBA VU "/home/juan/Desktop/arma.png"

enUnCanio :: ImagenDelCanio -> IO ImagenDelCanio
enUnCanio nico = (\enUnCanio -> decorarDelCanio (0,0) (1.5, 1.5) Behind enUnCanio nico) <$> readImageRGBA VU "/home/juan/Desktop/enUnCanio.jpg"

deAntanio :: ImagenDelCanio -> ImagenDelCanio
deAntanio (ImagenDelCanio imagenDelCanio bb) = ImagenDelCanio (G.map sepiaPixel imagenDelCanio) bb

fumandoseUnCanio :: ImagenDelCanio -> IO ImagenDelCanio
fumandoseUnCanio nico = (\fumandoseUnCanio -> decorarDelCanio (90, 75) (0.2, 0.2) OnTop fumandoseUnCanio nico) <$> readImageRGBA VU "/home/juan/Desktop/fumandoseUnCanio.png"

sepiaPixel :: Pixel RGBA Double -> Pixel RGBA Double
sepiaPixel unPixel = fromComponents (rojoSepia, verdeSepia, azulSepia, alphaOriginal)
    where rojoOriginal = getPxC unPixel RedRGBA
          verdeOriginal = getPxC unPixel GreenRGBA
          azulOriginal = getPxC unPixel BlueRGBA
          rojoSepia = min 1 ((0.393 * rojoOriginal + 0.769 * verdeOriginal + 0.189 * azulOriginal) * 1.1)
          verdeSepia = min 1 ((0.349 * rojoOriginal + 0.686 * verdeOriginal + 0.168 * azulOriginal) * 1.1)
          azulSepia = min 1 ((0.272 * rojoOriginal + 0.534 * verdeOriginal + 0.131 * azulOriginal) * 0.9)
          alphaOriginal = getPxC unPixel AlphaRGBA

data ImagenDelCanio = ImagenDelCanio { imagen :: Imagen, boundingBoxDelCanio :: BoundingBox }

mostrar :: ImagenDelCanio -> IO ()
mostrar = displayImage . imagen

mostrarConBB :: ImagenDelCanio -> IO ()
mostrarConBB (ImagenDelCanio imagen bb) = displayImage $ mergeImages (drawBoundingBox bb) imagen