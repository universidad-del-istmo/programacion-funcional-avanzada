module Javier.Ejercicios where

import Main
-- Ejercicio: Proveer una implementacion propia
-- de la funcion "forM"

forMJavier :: [a] -> (a -> b) -> IO [b]
forMJavier lista funcion = return (map funcion lista)