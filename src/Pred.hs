module Pred (
  Pred,
  cambiar, 
  anyFig, 
  allFig, 
  orP, 
  andP, 
  falla
) where
import Dibujo (Dibujo, figura, foldDib, change)


-- `Pred a` define un predicado sobre figuras básicas. Por ejemplo,
-- `(== Triangulo)` es un `Pred TriOCuat` que devuelve `True` cuando la
-- figura es `Triangulo`.
type Pred a = a -> Bool

-- Dado un predicado sobre figuras básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Figura x))` rota
-- todos los triángulos.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f dib = change (\x -> if p x then f x else figura x) dib

-- Alguna figura satisface el predicado.
anyFig :: Pred a -> Dibujo a -> Bool
anyFig p dib = foldDib (\x -> p x) id id id (\i j xs ys -> xs || ys) (\i j xs ys -> xs || ys) (\xs ys -> xs || ys) dib

-- Todas las figuras satisfacen el predicado.
allFig :: Pred a -> Dibujo a -> Bool
allFig p dib = foldDib (\x -> p x) id id id (\i j xs ys -> xs && ys) (\i j xs ys -> xs && ys) (\xs ys -> xs && ys) dib

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP p1 p2 a = p1 a && p2 a 

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p1 p2 a = p1 a || p2 a 

falla = True