-- Nombre: Juan Diego Vega

module Vega.Ejercicios where
import Control.Monad (forM, forM_)
-- Ejercicio: Proveer una implementacion propia
-- de la funcion "forM"
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM [] _ = return []
forM (x:xs) f = do
  y <- f x
  ys <- forM xs f
  return (y:ys)