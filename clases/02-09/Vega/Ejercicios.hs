-- Nombre: Juan Diego Vega

module Vega.Ejercicios where

-- Tarea: implementar "sum" utilizanjdo fold y hacer la reduccion de "sum" cuando implementa mediante un fold
miSum [] = 0
miSum (x:x2) = x + foldl (+) 0 x2
{- additionReductionFold :: [Int] -> Int
additionReductionFold = foldl (+) 0 -}

-- Tarea: implementen "reverse" utilizando fold.
-- Puede utilizar la lfuncion "adjuntar" ya sea
-- en el agregador o en el vacio.
adjuntar :: a -> [a] -> [a]
adjuntar x [] = [x]
adjuntar x (y:ys) = y : adjuntar x ys

reverseWithFold :: [a] -> [a]
reverseWithFold = foldl adjuntar []
