module Interp
  ( interp,
    initial,
  )
where

import Dibujo
import FloatingPic (half, FloatingPic)
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


-- FloatingPic :: Vector -> Vector -> 

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov p q = undefined


-- f(d+(w+h)/2, (w+h)/2, (h-w)/2)
r45 :: FloatingPic -> FloatingPic
r45 fp d w h = fp (d + half (w+h)) (half (w+h)) (half (h-w)) 

-- f(d+w, h, -w)
rot :: FloatingPic -> FloatingPic
rot fp d w h = fp (d+w) h (-w)

-- f(d+w, -w, h)
esp :: FloatingPic -> FloatingPic
esp fp d w h = fp (d+w) (-w) h 

-- f(d, w, h) ∪ g(d, w, h)
sup :: FloatingPic -> FloatingPic -> FloatingPic
sup fp1 fp2 d w h = ov (fp1 d w h) (fp2 d w h)

--f(x, w', h) ∪ g(d+w', r'*w, h) 
-- r=m/(m+n), r'=n/(m+n), w'=r*w
jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun r r' fp1 fp2 d w h = ov (fp1 d (r*w) h) (fp2 (d+r*w) (r'*w) h) 

-- f(d + h', w, r*h) ∪ g(d, w, h') con r' = n/(m+n), r=m/(m+n), h'=r'*h
api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api r r' fp1 fp2 d w h = ov (fp1 (d+(r'*h)) w (r*h)) (fp2 d w (r'*h))

-- type Output a = a -> FloatingPic
interp :: Output a -> Output (Dibujo a)
interp b = undefined