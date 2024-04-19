module Dibujos.Grilla (
    grilla,
    grillaConf
) where

import Dibujo (Dibujo, figura, juntar, apilar)
import Graphics.Gloss ( Picture, text, translate, color, black, red )
import FloatingPic (Output, Conf(..))

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

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
interpBasicaSinColor Parentesis (x1,y1) (x2,y2) (x3,y3) = translate x1 y1 $ text "(0,0)"


interpBas :: Output Basica
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

parentesisNegro :: BasicaSinColor -> Dibujo Basica
parentesisNegro b = figura (b, Negro)

generarGrilla :: Dibujo Basica
generarGrilla = grilla [ 
        [parentesisNegro Parentesis, parentesisNegro Parentesis, parentesisNegro Parentesis],
        [parentesisNegro Parentesis, parentesisNegro Parentesis]
        ]

grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla"
    , pic = generarGrilla
    , bas = interpBas
}
