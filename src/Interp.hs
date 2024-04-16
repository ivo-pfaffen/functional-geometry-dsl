module Interp
  ( interp,
    initial,
  )
where

import Dibujo (Dibujo (..), foldDib)
import FloatingPic 
  -- ( FloatingPic,
  --   Output,
  --   Conf (..),
  --   zero,
  --   half,
  --   hlines,
  --   grid,
  --   vacia,
  -- )
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


-- FloatingPic :: Vector -> Vector -> Vector -> Picture

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov p1 p2 = pictures [p1, p2] 

-- f(d+(w+h)/2, (w+h)/2, (h-w)/2)
r45 :: FloatingPic -> FloatingPic
r45 fp d w h = fp (d V.+ half (w V.+ h)) (half (w V.+ h)) (half (h V.- w)) 

-- f(d+w, h, -w)
rot :: FloatingPic -> FloatingPic
rot fp d w h = fp (d V.+ w) h (zero V.- w)

-- f(d+w, -w, h)
esp :: FloatingPic -> FloatingPic
esp fp d w h = fp (d V.+ w) (zero V.- w) h

-- f(d, w, h) ∪ g(d, w, h)
sup :: FloatingPic -> FloatingPic -> FloatingPic
sup fp1 fp2 d w h = ov (fp1 d w h) (fp2 d w h)

--f(x, w', h) ∪ g(d+w', r'*w, h) 
-- r=m/(m+n), r'=n/(m+n), w'=r*w
jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun m n fp1 fp2 d w h = (fp1 d (r V.* w) h) 
                        `ov`
                        (fp2 (d V.+ r V.* w) (r' V.* w) h) 
    where 
      r = m / (m + n) 
      r' = n / (m + n)
-- f(d + h', w, r*h) ∪ g(d, w, h') con r' = n/(m+n), r=m/(m+n), h'=r'*h
api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api m n fp1 fp2 d w h = (fp1 (d V.+ (r' V.* h)) w (r V.* h)) 
                        `ov` 
                        (fp2 d w (r' V.* h)) 
    where 
      r = m / (m + n) 
      r' = n / (m + n)

-- type Output a = a -> FloatingPic 

-- interp Output a -> Output (Dibujo a) 
-- = Output a -> Dibujo a -> FloatingPic 

interp :: Output a -> Output (Dibujo a)
interp f a = foldDib f rot esp r45 api jun sup a