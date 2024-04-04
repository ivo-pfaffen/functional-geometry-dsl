{-# LANGUAGE ExistentialQuantification #-}
module InterpHaha where

import qualified Data.Map as M
import Dibujo
import GHC.Float
import Graphics.Ascii.Haha.Bitmap
import Graphics.Ascii.Haha.Geometry
import Graphics.Ascii.Haha.Plot
import Graphics.Ascii.Haha.Terminal


type FloatingPicH = Char -> Point Double -> Point Double -> Point Double -> Bitmap Double Pixel
type OutputH a = a -> FloatingPicH

-- Configuración de la interpretación
data ConfH = forall a . ConfH
  { name :: String,
    pic :: Dibujo a,
    bas :: OutputH a
  }

-- todas las figuras se interpretan como rectángulos.
simpleHaha :: String -> Dibujo a -> ConfH
simpleHaha n p = ConfH {
  name = n ,
  pic = p ,
  bas = const rect
}

initialH :: ConfH -> Int -> IO ()
initialH (ConfH _ dib intBas) size = do
  putStr $ move 1 (1::Int)
  putStr $ plot $ fig '*' center w h
  where
    center :: Point Double
    center = Point (sF * 5) (sF * 5)
    s :: Integer
    s = toEnum size
    sF :: Double
    sF = toEnum size
    screen :: Rect Integer
    screen = Rect (Point 1 1) (Point 120 80)
    w = Point (sF * 5) 0
    h = Point 0 (sF * 3)
    view :: Rect Double
    view = Rect (Point 1 1) (Point 120 80)
    fig = interp intBas dib

    plot :: Bitmap Double Pixel -> String
    plot = string True screen (Point 1 1) " " "" . list 1 view

initialH' :: [ConfH] -> String -> IO ()
initialH' [] n = do
  putStrLn $ "No hay un dibujo llamado " ++ n
initialH' (c : cs) n =
  if n == name c
    then
      initialH c 8
    else
      initialH' cs n


-- el vector nulo
zero :: Point Double
zero = Point 0 0

half :: Point Double -> Point Double
half = scale 0.5 zero

infixl 4 .+

infixr 5 .*

(.+) :: Point Double -> Point Double -> Point Double
Point px py .+ Point qx qy = Point (px + qx) (py + qy)

(.-) :: Point Double -> Point Double -> Point Double
Point px py .- Point qx qy = Point (px - qx) (py - qy)

(.*) :: Double -> Point Double -> Point Double
(.*) = flip scale zero

toFlex :: (Fractional u, Ord u, Enum u) => Char -> Poly u -> Bitmap u Pixel -> Bitmap u Pixel
toFlex char = flip drawPoly (Pixel char magentaBold)

-- figuras adaptables comunes
trian :: FloatingPicH 
trian char a b c = toFlex char (Poly $ map (a .+) [zero, b, c, zero]) empty
rect :: Char -> Point Double -> Point Double -> Point Double -> Bitmap Double Pixel
rect char a b c = toFlex char (Poly . map (a .+) $ [zero, b, b .+ c, c, zero]) empty

-- Interpretación de (^^^)
drawLayers :: (Ord p) => [Bitmap p u] -> Bitmap p u
drawLayers = Bitmap . M.unions . map bits

ov :: (Ord p) => Bitmap p u -> Bitmap p u -> Bitmap p u
ov p q = drawLayers [p, q]

r45 :: FloatingPicH -> FloatingPicH
r45 = undefined

rot :: FloatingPicH -> FloatingPicH
rot = undefined

esp :: FloatingPicH -> FloatingPicH
esp = undefined

sup :: FloatingPicH -> FloatingPicH -> FloatingPicH
sup = undefined

jun :: Float -> Float -> FloatingPicH -> FloatingPicH -> FloatingPicH
jun = undefined

api :: Float -> Float -> FloatingPicH -> FloatingPicH -> FloatingPicH
api = undefined

interp :: OutputH a -> OutputH (Dibujo a)
interp b = undefined

