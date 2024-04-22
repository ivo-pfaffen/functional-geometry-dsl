module Dibujos.Escher where

import Dibujo (Dibujo, figura, espejar, rot45, r90, rotar, encimar, cuarteto)
import FloatingPic(Conf(..), Output, half)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss (Picture, black, color, line, pictures)

data Color = Negro
    deriving (Show, Eq)

data BasicaSinColor = Triangulo | Rectangulo
    deriving (Show, Eq)

type Escher = (BasicaSinColor, Color)

-- Figura basica 
colorear :: Color -> Picture -> Picture
colorear Negro = color black

--  x + w
--  | 
--  x --- x + y
interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), half y V.+ half w, y, (0,0)] -- (vers. triangulo 2)
interpBasicaSinColor Rectangulo x y w = line [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
-- interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), y V.+ half w, w, (0,0)]

interpBas :: Output Escher
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

-- base <-> fish en el paper de Henderson
base :: Escher -> Dibujo Escher
base e = figura e

base2 :: Dibujo Escher -> Dibujo Escher
base2 e = espejar (rot45 e) 

base3 :: Dibujo Escher -> Dibujo Escher
base3 e = rotar (rotar (rotar (base2 e))) 


-- El dibujo u: over(over(fish2, rot(fish2)), over(rot(rot(fish2)), rot(rot(rot(fish2))))
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU e = encimar (encimar (base2 e) (rotar (base2 e))) (encimar (rotar (rotar (base2 e)))  (rotar(rotar(rotar (base2 e)))))

-- El dibujo t: over(fish, over(fish2, fish3))
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT e = encimar e (encimar (base3 e) (base2 e))

blank :: Dibujo Escher
blank = base (Rectangulo, Negro)

-- blank <- Rectangulo
-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto blank blank blank (dibujoU p)
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (rotar (lado (n-1) p)) (dibujoU p)

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto blank blank (rotar (dibujoT p)) (dibujoT p)
lado n p = cuarteto (lado (n-1) p) (lado (n-1) p) (rotar p) p


-- Por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = undefined

-- El dibujo de Escher. Recursivo. 
escher :: Int -> Escher -> Dibujo Escher
escher i e = base e


escherConf :: Conf
escherConf = Conf {
    name = "Escher"
    , pic = lado 8 (base (Triangulo, Negro))
    , bas = interpBas
}