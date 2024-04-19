module Dibujos.Grilla (
    grilla,
    grillaConf
) where

import Dibujo (Dibujo, figura, juntar, apilar)
import Graphics.Gloss ( Picture, text, color, black, red )
import FloatingPic (Output, Conf(..))

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar (fromIntegral $ length ds) 1 d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar (fromIntegral $ length ds) 1 d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

data Color = Negro | Rojo
    deriving(Show, Eq)

data BasicaSinColor = Parentesis | Numero | Coma
    deriving(Show, Eq)

type Basica = (BasicaSinColor, Color)

colorear :: Color -> Picture -> Picture
colorear Negro = color black
colorear Rojo = color red

interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Parentesis x y w = text "("

interpBas :: Output Basica
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

parentesisNegro :: BasicaSinColor -> Dibujo Basica
parentesisNegro b = figura (b, Negro)

generarGrilla :: Dibujo Basica
generarGrilla = grilla [
    [parentesisNegro Parentesis, parentesisNegro Parentesis],
    [parentesisNegro Parentesis, parentesisNegro Parentesis]
    ]

grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla"
    , pic = generarGrilla
    , bas = interpBas
}