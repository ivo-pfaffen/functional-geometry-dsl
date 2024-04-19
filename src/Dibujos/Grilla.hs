module Dibujos.Grilla (
    grilla,
    grillaConf
) where

import Dibujo (Dibujo, figura, juntar, apilar)
import Graphics.Gloss ( Picture, text, translate, scale, color, black, red )
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

type Basica = String

interpBas :: Output Basica
interpBas b (x1,y1) (x2,y2) (x3,y3) = translate x1 (y1+42) $ scale 0.15 0.15 $ text b

coordenada :: Basica -> Dibujo Basica
coordenada s = figura s

texto :: String -> String -> String
texto s1 s2 = "(" ++ s1 ++ "," ++ s2 ++ ")"

generarFila :: Int -> Int -> [String]
generarFila row 0 = [texto (show row) (show 0)]
generarFila row col = generarFila row (col-1) ++ [texto (show row) (show col)]

generarColumna :: [Basica] -> [Dibujo Basica]
generarColumna = map coordenada

generarGrilla:: Int -> Int -> [[Dibujo Basica]]
generarGrilla 0 col = [generarColumna (generarFila 0 col)]
generarGrilla row col =  generarGrilla (row-1) col ++ [generarColumna (generarFila row col)]

grillaEjemplo :: Dibujo Basica
grillaEjemplo = grilla (generarGrilla 7 7) 


grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla"
    , pic = grillaEjemplo
    , bas = interpBas
}
