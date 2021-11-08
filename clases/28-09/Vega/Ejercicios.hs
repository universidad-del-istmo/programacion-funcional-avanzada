-- Nombre: Juan Diego Vega

module Vega.Ejercicios where

    -- Ejercicio:
-- Defina la instancia de Functor (solo fmap)
-- para el ArbolBinario
instance Functor ArbolBinario where
    fmap f comodin = comodin
    fmap f (nodo alpha) = nodo (f alpha)
    fmap f (branch alpha beta charly) = branch (f alpha) (fmap f beta) (fmap f charly)