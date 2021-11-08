-- Nombre: Juan Diego Vega

module Vega.Ejercicios where

-- Dar una definicion propioa de la
-- funcion take

-- miTake n lista = undefined
miTake :: Int -> [a] -> [a]
miTake _ [] = []
miTake 0 _ = []
miTake n (x:lista) = x:miTake (n-1) lista 

-- Dar una definicion propioa de la
-- funcion drop

-- miDrop n lista = undefined
miDrop :: Int -> [a] -> [a]
miDrop _ [] = []
miDrop 0 lista = lista
miDrop n (_:lista) = miDrop (n-1) lista