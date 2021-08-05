module Main where

-- Definicion de listas
simple = [1,2,3,4,5]

-- Rango
rango = [1 .. 5]
rangoGrande = [1 .. ]

-- Comprensiones
comprension = [ i * i | i <- rango]

comprensionCompleja = [(i, j) | i <-rango, j <- comprension]

-- Array vs Lista Encadenada

-- Un array es un puntero
-- Accessar el valor i del array
-- consiste en *(array + i)

-- Una lista encadenada es una
-- secuencia de punteros
-- [1,2,3,4,5]
-- 1 -> 2 -> 3 -> 4 -> 5 -> NULL

getValue lista i =
    if i == 0
    then head lista
    else getValue resto (i - 1)

    where
        resto = tail lista

agregarAlFinal lista valor =
    if null lista
    then valor : []
    else
        primero : agregarAlFinal resto valor

    where
        primero = head lista
        resto = tail lista

concatenar l1 l2 =
    if null l2
    then l1
    else
        concatenar (agregarAlFinal l1 l) resto 

    where
        l = head l2
        resto = tail l2

main :: IO ()
main = putStrLn "Hello, Haskell!"
