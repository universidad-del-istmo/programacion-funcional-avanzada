module Main where

import System.Posix.Internals (setNonBlockingFD)
voltearPareja (x, y) = (y, x)

voltearParejas [] = []
voltearParejas ((x, y) : xs) =
    (y, x) : voltearParejas xs

todosTrue [] = True
todosTrue (True : xs) = todosTrue xs
todosTrue _ = False

sonIguales [] [] = True
sonIguales (x : xs) (y : ys) = x == y && sonIguales xs ys
sonIguales _ _ = False

factorialConGuardias n
    | n == 0 || n == 1 = 1
    | n == 2 = 2
    | otherwise = n * factorialConGuardias (n - 1)

sonIgualesGuardias [] [] = True
sonIgualesGuardias (x : xs) (y : ys)
    | x == y = sonIgualesGuardias xs ys
    | otherwise = False
sonIgualesGuardias _ _ = False

eliminarValor _ [] = []
eliminarValor v (x : xs)
    | x == v = eliminarValor v xs
    | otherwise = x : eliminarValor v xs

merge [] [] = []
merge (x : xs) (y : ys)
    | x < y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys
merge [] ys = ys
merge xs [] = xs

mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    merge (mergeSort xs1) (mergeSort xs2)
    where
        l = length xs
        n = div l 2
        xs1 = take n xs
        xs2 = drop n xs

quickSort [] = []
quickSort (x : xs) =
    quickSort menores ++ [x] ++ quickSort mayores
    where
        menores = [i | i <- xs, i < x]
        mayores = [i | i <- xs, i >= x]

-- Tarea
-- definir la funcion "convertirAModulo"
-- Esta funcion debe tomar una lista de
-- enteros y un entero llamado "m" que representa
-- un modulo. Esta funcion debe retornar una
-- nueva lista donde todos los valores de la lista
-- origianl se convierten a su valor equivalente
-- en el modulo.

-- Tarea
-- Definir la funcio "sumaModularDeListas"
-- esta funcion debe aceptar dos listas
-- de enteros y un modulo. Debe producir
-- una nueva lista donde los valores con
-- el mismo indice son sumados entre elos.
-- Ej.
-- sumaModularDeListas [1,2] [4,5] 3 == [mod (1 + 4) 3, mod (2 + 5) 3]

-- Tarea
-- Definir la funcion "igualdadDeListasModular"
-- Esta funcion acepta dos listas de numeros y
-- un modulo "m". Debe retornar True si las dos
-- listas son iguales en ese modulo
-- igualdadDeListasModular 5 [1,2,3] [6,7,8] == True
-- igualdadDeListasModular 5 [1,2,3] [6,7,9] == False

-- Tarea
-- Definir la funcion "existeModular"
-- Esta funcion toma un modulo "m", un valor "v"
-- y una lista de valores. Retorna true si la
-- lista tiene un valor equivalente a "v" en el
-- modulo "m"
-- Ej:
-- existeModular 5 8 [1,2,3] == True
-- existeModular 5 8 [16,17,18] == True
-- existeMOdular 5 8 [1,2,9] == False


main :: IO ()
main = putStrLn "Hello, Haskell!"
