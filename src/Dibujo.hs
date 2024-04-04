module Dibujo (encimar, 
    -- agregar las funciones constructoras
    ) where


-- nuestro lenguaje 
data Dibujo a = Dibujo

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp n f = undefined


-- Funciones constructoras
figura :: a -> Dibujo a
figura = undefined

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = undefined

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = undefined

-- juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
-- juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = undefined

rotar :: Dibujo a -> Dibujo a
rotar = undefined


espejar :: Dibujo a -> Dibujo a
espejar = undefined

(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = undefined

(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = undefined

(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = undefined

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = undefined

r180 :: Dibujo a -> Dibujo a
r180 = undefined

r270 :: Dibujo a -> Dibujo a
r270 = undefined

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 = undefined

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto = undefined

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar = undefined

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib = undefined
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change = undefined

-- Principio de recursión para Dibujos.
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
foldDib = undefined