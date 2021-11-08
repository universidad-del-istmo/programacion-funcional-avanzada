-- Nombre: Juan Diego Vega

module Vega.Ejercicios where

-- Ejercicio
-- Definir la funcion "range", esta funcion
-- toma un numero inicial y una cantidad y
-- enumera "cantidad" numeros en una lista
-- empezando por "inicial" e incrementando
-- por uno cada valor
range initial quantity =
    if quantity == 0
    then []
    else initial : range (initial + 1) (quantity - 1)

-- Ejercicio: Definir la funcion "take"
-- esta funcion toma una lista y un
-- numero. Produce una nueva lista donde
-- que tiene los primeros "n" numeros de
-- la lista original.
myTake [] _ = []
myTake (x:xs) n =
    if n == 0
    then []
    else x : myTake xs (n - 1)