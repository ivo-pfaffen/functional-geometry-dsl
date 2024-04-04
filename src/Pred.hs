module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP, falla
) where

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar = undefined


-- Alguna básica satisface el predicado.
anyDib = undefined

-- Todas las básicas satisfacen el predicado.
allDib = undefined

-- Los dos predicados se cumplen para el elemento recibido.
andP = undefined

-- Algún predicado se cumple para el elemento recibido.
orP = undefined

falla = True