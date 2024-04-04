module Interp
  ( interp,
    initial,
  )
where

import Dibujo
import FloatingPic
import Graphics.Gloss (Display (InWindow), color, display, makeColorI, pictures, translate, white, Picture)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial (Conf n dib intBas) size = display win white $ withGrid fig size
  where
    win = InWindow n (ceiling size, ceiling size) (0, 0)
    fig = interp intBas dib (0, 0) (size, 0) (0, size)
    desp = -(size / 2)
    withGrid p x = translate desp desp $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 100 100 100 100

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov p q = undefined

r45 :: FloatingPic -> FloatingPic
r45 = undefined

rot :: FloatingPic -> FloatingPic
rot = undefined

esp :: FloatingPic -> FloatingPic
esp = undefined

sup :: FloatingPic -> FloatingPic -> FloatingPic
sup = undefined

jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun = undefined

api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api = undefined

interp :: Output a -> Output (Dibujo a)
interp b = undefined