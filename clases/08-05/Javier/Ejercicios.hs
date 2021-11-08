module Javier.Ejercicios where

-- Dar una definicion propioa de la
-- funcion take
javierTake :: Int -> [a] -> [a]
javierTake 0 _ = []
javierTake n lista = elemento : javierTake (n - 1) resto
    where
        elemento = head lista
        resto = tail lista

-- Dar una definicion propioa de la
-- funcion drop

miDrop 0 lista = lista 
miDrop n lista = 
    if n == 0 then 
        resto 
    else 
        miDrop (n - 1) resto
    where
        resto = tail lista