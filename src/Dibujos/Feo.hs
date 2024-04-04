module Dibujos.Feo where

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, encimar, espejar)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, blue, red, color, line, pictures )

-- Les ponemos colorcitos para que no sea _tan_ feo
data Color = Azul | Rojo
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo | Cruz | Triangulo | Efe
    deriving (Show, Eq)

type Basica = (BasicaSinColor, Color)

colorear :: Color -> Picture -> Picture
colorear Azul = color blue
colorear Rojo = color red

-- Las coordenadas que usamos son:
--
--  x + y
--  |
--  x --- x + w
--
-- por ahi deban ajustarlas
interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Rectangulo x y w = line [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
interpBasicaSinColor Cruz x y w = pictures [line [x, x V.+ y V.+ w], line [x V.+ y, x V.+ w]]
interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), y V.+ half w, w, (0,0)]
interpBasicaSinColor Efe x y w = line . map (x V.+) $ [
        zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY,
        uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5,
        x4 V.+ 6 V.* uY, 6 V.* uY, zero
    ]
    where
        p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* y
        uY = (1/6) V.* w

interpBas :: Output Basica
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w


-- Diferentes tests para ver que estén bien las operaciones
figRoja :: BasicaSinColor -> Dibujo Basica
figRoja b = figura (b, Rojo)

figAzul :: BasicaSinColor -> Dibujo Basica
figAzul b = figura (b, Azul)

-- Debería mostrar un rectángulo azul arriba de otro rojo,
-- conteniendo toda la grilla dentro
apilados :: BasicaSinColor -> Dibujo Basica
apilados b = apilar 1 1 (figAzul b) (figRoja b)

-- Debería mostrar un rectángulo azul arriba de otro rojo,
-- conteniendo toda la grilla dentro el primero ocupando 3/4 de la grilla
apilados2 :: BasicaSinColor -> Dibujo Basica
apilados2 b = apilar 3 1 (figAzul b) (figRoja b)

-- Debería mostrar un rectángulo azul a derecha de otro rojo,
-- conteniendo toda la grilla dentro
juntados :: BasicaSinColor -> Dibujo Basica
juntados b = juntar 1 1 (figAzul b) (figRoja b)

-- Debería mostrar un rectángulo azul a derecha de otro rojo,
-- conteniendo toda la grilla dentro el primero ocupando 3/4 de la grilla
juntados2 :: BasicaSinColor -> Dibujo Basica
juntados2 b = juntar 3 1 (figAzul b) (figRoja b)

-- Igual al anterior, pero invertido
flipante1 :: BasicaSinColor -> Dibujo Basica
flipante1 b = espejar $ juntados2 b

-- Igual al anterior, pero invertido
flipante2 :: BasicaSinColor -> Dibujo Basica
flipante2 b = espejar $ apilados2 b

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

cruzTangulo :: Dibujo Basica
cruzTangulo = encimar (figRoja Rectangulo) (figAzul Cruz)

efe :: Dibujo Basica
efe = figura (Efe, Azul)

testAll :: Dibujo Basica
testAll = grilla [
    [cruzTangulo         , rot45 cruzTangulo   , efe                , rot45 efe                ],
    [apilados Rectangulo , apilados2 Rectangulo, juntados Rectangulo, juntados2 Rectangulo     ],
    [flipante1 Rectangulo, flipante2 Rectangulo, figRoja Triangulo  , rotar $ figAzul Triangulo],
    [rotar $ apilados Efe, apilados2 Efe       , juntados Efe       , juntados2 Efe            ]
    ]

feoConf :: Conf
feoConf = Conf {
    name = "Feo"
    , pic = testAll
    , bas = interpBas
}