module Dibujos.Escher where

import Dibujo (Dibujo, figura, espejar, rot45, r180, r270, rotar, encimar, cuarteto, apilar, juntar)
import FloatingPic(Conf(..), Output, half)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss (Picture(Blank), black, color, line, polygon, pictures)

data Color = Negro
    deriving (Show, Eq)

data BasicaSinColor = Triangulo | Rectangulo | Vacio
    deriving (Show, Eq)

type Escher = (BasicaSinColor, Color)

-- Figura basica 
colorear :: Color -> Picture -> Picture
colorear Negro = color black

--  x + w
--  | 
--  x --- x + y
interpBasicaSinColor :: Output BasicaSinColor
-- interpBasicaSinColor Triangulo x y w = polygon $ map (x V.+) [(0,0), half y V.+ half w, y, (0,0)] -- Triángulo mirando arriba
-- interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), y V.+ half w, w, (0,0)]         -- Triangulo acostado
-- interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), w, y, (0,0)]                    -- Triangulo rectángulo normal
interpBasicaSinColor Triangulo x y w = pictures [(line $ map (x V.+) [(0,0), w, y, (0,0)]), (polygon $ map (x V.+) [(0,0), half w, half y, (0,0)])] -- Triangulo rectángulo decorado
interpBasicaSinColor Rectangulo x y w = line [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
interpBasicaSinColor Vacio _ _ _ = Blank

interpBas :: Output Escher
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

-- base <-> fish en el paper de Henderson
base :: Escher -> Dibujo Escher
base e = figura e

base2 :: Dibujo Escher -> Dibujo Escher
base2 e = espejar (rot45 e) 

base3 :: Dibujo Escher -> Dibujo Escher
base3 e = r270 (base2 e)


-- El dibujo u: over(over(fish2, rot(fish2)), over(rot(rot(fish2)), rot(rot(rot(fish2))))
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU e = encimar (encimar (base2 e) (rotar (base2 e))) (encimar (r180 (base2 e)) (r270 (base2 e)))

-- El dibujo t: over(fish, over(fish2, fish3))
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT e = (encimar (encimar (base3 e) (base2 e)) e)

blank :: Dibujo Escher
blank = base (Vacio, Negro)

-- blank <- Rectangulo
-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Escher -> Dibujo Escher
esquina 1 p = cuarteto blank blank blank (dibujoU (figura p))
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (rotar (lado (n-1) p)) (dibujoU (figura p))

-- Lado con nivel de detalle. Según la consigna:
-- lado(1, f) = cuarteto(blank, blank, rotar(dibujo_t(f)), dibujo_t(f))
-- lado(2, f) = cuarteto(lado(1, f), lado(1, f), rotar(f), f)
-- para lograr un mejor efecto, modifico los últimos dos argumentos del caso recursivo (uso dibujoT en vez de la figura base)
lado :: Int -> Escher -> Dibujo Escher
lado 1 p = cuarteto blank blank (rotar (dibujoT (figura p))) (dibujoT (figura p))
lado n p = cuarteto (lado (n-1) p) (lado (n-1) p) (rotar (dibujoT (figura p))) (dibujoT (figura p))

-- Modularizo fila del combinador del dibujo
filaCombinador :: Dibujo Escher -> Dibujo Escher -> Dibujo Escher -> Dibujo Escher
filaCombinador e1 e2 e3 = juntar (2/3) (1/3) (juntar (1/2) (1/2) e1 e2) e3

-- El combinador del dibujo
-- Ahora es simétrico. Anteriormente estaba implementado usando cuartos (para seguir estrictamente el documento).
noneto p q r s t u v w x = apilar (1/3) (2/3) (filaCombinador p q r) (apilar (1/2) (1/2) (filaCombinador s t u) (filaCombinador v w x)) 

-- Dibujo de Escher
escher :: Int -> Escher -> Dibujo Escher
-- P Q R                    
-- S T U                  
-- V W X
escher i e =  noneto (esquina i e)         (lado i e)               (espejar (esquina i e)) 
              (rotar (lado i e))       (dibujoU (figura e))        (espejar (rotar (lado i e))) 
             (rotar (esquina i e))      (r180 (lado i e))         (espejar (rotar (esquina i e)))

escherConf :: Conf
escherConf = Conf {
    name = "Escher"
    , pic = escher 6 (Triangulo, Negro)
    , bas = interpBas
}