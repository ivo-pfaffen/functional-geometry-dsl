{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module InterpSVG where

import Data.Text (pack)
import Dibujo
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss.Data.Vector (Vector)
import Lucid.Svg
  ( Svg,
    With (with),
    d_,
    doctype_,
    g_,
    height_,
    lA,
    lR,
    mA,
    path_,
    svg11_,
    version_,
    width_,
    z,
  )
import Lucid.Svg.Attributes (fill_, stroke_, transform_)

type FloatingPicS = Vector -> Vector -> Vector -> Svg ()

type OutputS a = a -> FloatingPicS

-- Configuración de la interpretación
data ConfSVG = forall a. ConfSVG
  { name :: String,
    pic :: Dibujo a,
    bas :: OutputS a
  }

initialSVG :: ConfSVG -> Float -> IO ()
initialSVG (ConfSVG n dib intBas) size = writeFile fname (show svg)
  where
    fname = n ++ ".svg"
    delta = show (600, 600)
    trans = pack $ "translate" ++ delta
    dibujo =
      with (g_ $ interp intBas dib (0, 0) (size, 0) (0, -size)) [transform_ trans]
    svg = do
      doctype_
      with (svg11_ dibujo) [version_ "1.1", width_ "1200", height_ "1200"]

initialSVG' :: [ConfSVG] -> String -> IO ()
initialSVG' [] n = do
  putStrLn $ "No hay un dibujo llamado " ++ n
initialSVG' (c : cs) n =
  if n == name c
    then
      initialSVG c 150
    else
      initialSVG' cs n

-- todas las figuras se interpretan como rectángulos.
simpleSVG :: String -> Dibujo a -> ConfSVG
simpleSVG n p =
  ConfSVG
    { name = n,
      pic = p,
      bas = const rect
    }

-- el vector nulo
zero :: Vector
zero = (0, 0)

half :: Vector -> Vector
half = (0.5 V.*)

vecToSvg :: Vector -> [Float]
vecToSvg (x, y) = [x, y]

-- lR' = uncurry lR
lR' = uncurry lR

-- figuras adaptables comunes
trian1 :: FloatingPicS
trian1 a b c = path_ [d_ cmds]
  where
    cmds = uncurry mA a <> lR 0 0 <> lR' (half b V.+ half c) <> lR' b <> lR 0 0 <> z

trian2 :: FloatingPicS
trian2 a b c = path_ [d_ cmds]
  where
    cmds = uncurry mA a <> lR 0 0 <> lR' b <> lR' c <> lR 0 0 <> z

rect :: FloatingPicS
rect a b c = with (path_ [d_ cmds]) [fill_ "transparent", stroke_ "black"]
  where
    cmds = uncurry mA a <> lR 0 0 <> lR' b <> lR' c <> lR' ((-1) V.* b) <> lR' ((-1) V.* c) <> z

void :: FloatingPicS
void _ _ _ = ""

r45 :: FloatingPicS -> FloatingPicS
r45 = undefined

rot :: FloatingPicS -> FloatingPicS
rot = undefined

esp :: FloatingPicS -> FloatingPicS
esp = undefined

sup :: FloatingPicS -> FloatingPicS -> FloatingPicS
sup = undefined

jun :: Float -> Float -> FloatingPicS -> FloatingPicS -> FloatingPicS
jun = undefined

api :: Float -> Float -> FloatingPicS -> FloatingPicS -> FloatingPicS
api = undefined

interp :: OutputS a -> OutputS (Dibujo a)
interp = undefined
