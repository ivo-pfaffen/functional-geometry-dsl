{-# LANGUAGE ExistentialQuantification #-}
module FloatingPic
  ( FloatingPic,
    Output,
    Conf (..),
    zero,
    half,
    hlines,
    grid,
    vacia,
  )
where

import Dibujo (Dibujo)
import Graphics.Gloss (Picture, Vector, blank, line, pictures, rotate, translate)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

type FloatingPic = Vector -> Vector -> Vector -> Picture

type Output a = a -> FloatingPic

-- Configuración de la interpretación
data Conf = forall a . Conf 
  { name :: String,
    pic :: Dibujo a,
    bas :: Output a
  }

-- El vector nulo
zero :: Vector
zero = (0, 0)

-- Escala el argumento por 0.5
half :: Vector -> Vector
half = (0.5 V.*)

-- Infinitas lineas paralelas horizontales
-- Desde (x, y) para arriba con un largo de mag
hlines :: Vector -> Float -> Float -> [Picture]
hlines (x, y) mag sep = map hline [0, sep ..]
  where
    hline h = line [(x, y + h), (x + mag, y + h)]

-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l.
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls, translate 0 (l * toEnum n) (rotate 90 ls)]
  where
    ls = pictures $ take (n + 1) $ hlines v sep l

vacia :: FloatingPic
vacia _ _ _ = blank
