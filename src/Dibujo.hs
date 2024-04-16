module Dibujo (encimar, 
                figura, 
                rotar, 
                espejar, 
                rot45, 
                apilar, 
                juntar, 
                (^^^), 
                (.-.), 
                (///), 
                r90, 
                r180, 
                r270, 
                encimar4, 
                cuarteto, 
                ciclar, 
                mapDib, 
                change, 
                foldDib, 
                figuras, 
                Dibujo(..)
    ) where


-- nuestro lenguaje 
data Dibujo a = Figura a 
              | Rotar (Dibujo a)
              | Espejar (Dibujo a)
              | Rot45 (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
              deriving (Eq, Show)
-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

-- Composición n-veces de una función con sí misma. Componer 0 veces
-- es la función identidad, componer 1 vez es aplicar la función 1 vez, etc.
-- Componer negativamente es un error!
comp :: Int -> (a -> a) -> a -> a
comp 0 _ = id
comp n f  | n < 0      = error "No esta permitido componer negativamente"
          | otherwise  = f . (comp (n-1) f)

-- Funciones constructoras
figura :: a -> Dibujo a
figura = Figura 

rotar :: Dibujo a -> Dibujo a
rotar = Rotar 

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar  

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45 

espejar :: Dibujo a -> Dibujo a
espejar = Espejar 

-- Superpone un dibujo con otro.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) =  Encimar 

-- Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1.0 1.0 

-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1.0 1.0 


-- Rotaciones de múltiplos de 90.
r90 :: Dibujo a -> Dibujo a
r90 = comp 2 rot45
 
r180 :: Dibujo a -> Dibujo a
r180 = comp 2 r90  

r270 :: Dibujo a -> Dibujo a
r270 = comp 3 r90 

-- una figura repetida con las cuatro rotaciones, superimpuestas.
-- Un dibujo repetido con las cuatro rotaciones, superpuestos.
encimar4 :: Dibujo a -> Dibujo a
encimar4 dib = encimar dib (encimar (r90 dib) (encimar (r180 dib) (r270 dib))) 

-- cuatro figuras en un cuadrante.
-- Dados cuatro dibujos los ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto dib1 dib2 dib3 dib4 = (.-.) ((///) dib1  dib2) ((///) dib3  dib4)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
-- Cuadrado con el mismo dibujo rotado i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar dib =  (.-.) ((///) dib (r90 dib)) ((///) (r180 dib) (r270 dib))

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Figura x)  = Figura (f x)
mapDib f (Rotar dib)   = Rotar (mapDib f dib) 
mapDib f (Espejar dib) = Espejar (mapDib f dib)
mapDib f (Rot45 dib)   = Rot45 (mapDib f dib)
mapDib f (Apilar float1 float2 dib1 dib2) = Apilar float1 float2 (mapDib f dib1) (mapDib f dib2)
mapDib f (Juntar float1 float2 dib1 dib2) = Juntar float1 float2 (mapDib f dib1) (mapDib f dib2)
mapDib f (Encimar dib1 dib2) = Encimar (mapDib f dib1) (mapDib f dib2) 

-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f (Figura x)    = f x
change f (Rotar dib)   = Rotar (change f dib) 
change f (Espejar dib) = Espejar (change f dib)
change f (Rot45 dib)   = Rot45 (change f dib)
change f (Apilar float1 float2 dib1 dib2) = Apilar float1 float2 (change f dib1) (change f dib2)
change f (Juntar float1 float2 dib1 dib2) = Juntar float1 float2 (change f dib1) (change f dib2)
change f (Encimar dib1 dib2) = Encimar (change f dib1) (change f dib2) 


-- Principio de recursión para Dibujos.
-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de intro a la lógica
-- foldDib aplicado a cada constructor de Dibujo debería devolver el mismo
-- dibujo
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib fig _ _ _ _ _ _ (Figura x)  = fig x 
foldDib fig rot esp r45 api jun enc (Rotar dib)   = rot (foldDib fig rot esp r45 api jun enc dib) 
foldDib fig rot esp r45 api jun enc (Espejar dib) = esp (foldDib fig rot esp r45 api jun enc dib)
foldDib fig rot esp r45 api jun enc (Rot45 dib)   = r45 (foldDib fig rot esp r45 api jun enc dib)
foldDib fig rot esp r45 api jun enc (Apilar float1 float2 dib1 dib2) = api float1 float2 (foldDib fig rot esp r45 api jun enc dib1) (foldDib fig rot esp r45 api jun enc dib2)
foldDib fig rot esp r45 api jun enc (Juntar float1 float2 dib1 dib2) = jun float1 float2 (foldDib fig rot esp r45 api jun enc dib1) (foldDib fig rot esp r45 api jun enc dib2)
foldDib fig rot esp r45 api jun enc (Encimar dib1 dib2) = enc (foldDib fig rot esp r45 api jun enc dib1) (foldDib fig rot esp r45 api jun enc dib2)


-- figuras :: Dibujo a -> [a]
-- figuras (Figura x) = [x]
-- figuras (Rotar dib) = figuras dib
-- figuras (Espejar dib) = figuras dib
-- figuras (Rot45 dib) = figuras dib
-- figuras (Apilar float1 float2 dib1 dib2) = figuras dib1 ++ figuras dib2
-- figuras (Juntar float1 float2 dib1 dib2) = figuras dib1 ++ figuras dib2
-- figuras (Encimar dib1 dib2) = figuras dib1 ++ figuras dib2


figuras :: Dibujo a -> [a]
figuras dib = foldDib (\x -> [x]) id id id (\_ _ xs ys -> xs ++ ys) (\_ _ xs ys -> xs ++ ys) (\xs ys -> xs ++ ys) dib