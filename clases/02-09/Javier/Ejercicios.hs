module Javier.Ejercicios where

fold :: (estado -> a -> estado) -> estado -> [a] -> estado
fold fCons fNil [] = fNil
fold fCons fNil (x : xs) = fCons (fold fCons fNil xs) x

-- Tarea: implementar "sum" utilizanjdo fold
-- Tarea: Hacer la reduccion de "sum" cuando
-- implementa mediante un fold

sum' xs = fold agg vacio xs
    where
        agg estado x = estado + x
        vacio = 0

-- Tarea: implementen "reverse" utilizando fold.
-- Puede utilizar la lfuncion "adjuntar" ya sea
-- en el agregador o en el vacio.

adjuntar' a xs = fold agg vacio xs
    where
        agg estado x = x : estado
        vacio = [a]

reverse' xs = fold agg vacio xs
    where
        agg estado x = estado ++ [x] 
        vacio = []