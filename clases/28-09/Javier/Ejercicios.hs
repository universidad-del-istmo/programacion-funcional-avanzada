module Javier.Ejercicios where

import Main 

-- Ejercicio:
-- Defina la instancia de Functor (solo fmap)
-- para el ArbolBinario

instance Functor ArbolBinario where
    fmap f Nada = Nada
    fmap f (Hoja a) = Hoja (f a)
    fmap f (Rama a b c) = Rama (f a) (fmap f b) (fmap f c)