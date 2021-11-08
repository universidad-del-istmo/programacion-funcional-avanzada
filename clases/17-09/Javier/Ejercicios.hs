module Javier.Ejercicios where

import Main

-- Ejercicio
-- Definir la funcion "range", esta funcion
-- toma un numero inicial y una cantidad y
-- enumera "cantidad" numeros en una lista
-- empezando por "inicial" e incrementando
-- por uno cada valor
miRange :: Int -> Int -> Lista
miRange inicio cantidad = 
    if inicio > cantidad then
        Nil 
    else 
        Cons inicio (miRange (inicio + 1) cantidad)   

-- Ejercicio: Definir la funcion "take"
-- esta funcion toma una lista y un
-- numero. Produce una nueva lista donde
-- que tiene los primeros "n" numeros de
-- la lista original.
miTake :: Int -> Lista -> Lista
miTake 0 _ = Nil
miTake n (Cons x resto) = Cons x (miTake (n - 1) resto)