module Main where

import Data.Ratio ((%))

main :: IO ()
main = putStrLn "Hello 1, Haskell!"

---------------------------------------------------------- Ejercicio o Tarea #1 ----------------------------------------------------------
-- Definir una funcion que corresponda a la figura que se presenta a continuacion.
-- Comienza en (0,0)
-- 3 unidades horizontales
-- 3 unidades con pendiente hacia arriba
-- 3 unidades con pendiente hacia abajo
-- 3 unidades mas horizontales
--
--       /\
--      /  \
-- ____/    \____

linea :: Num a => a -> a -> a -> a -> a
linea m a b x = m*(x - a) + b

lineaHorizontal x = linea m a b
    where
        m = 0
        a = -3
        b = 0

lineaVerticalArriba x = linea m a b
    where
        m = 1
        a = -3
        b = 1

lineaVerticalAbajo x = linea m a b
    where
        m = -1
        a = -3
        b = -1

lineaCompuesta x
    | x < 9 = lineaHorizontal x
    | x < 18 = lineaVerticalArriba x
    | x < 27 = lineaVerticalAbajo x
    | otherwise = lineaHorizontal x

volcan inicio fin =
    [lineaCompuesta x | x <- [inicio .. fin]]

---------------------------------------------------------- Ejercicio o Tarea #2 ----------------------------------------------------------
-- Dar una definicion propia de la funcion take
miTake :: Int -> [a] -> [a]
miTake _ [] = []
miTake 0 _ = []
miTake n (x:lista) = x:miTake (n-1) lista 

---------------------------------------------------------- Ejercicio o Tarea #3 ----------------------------------------------------------
-- Dar una definicion propia de la funcion drop
miDrop :: Int -> [a] -> [a]
miDrop _ [] = []
miDrop 0 lista = lista
miDrop n (_:lista) = miDrop (n-1) lista

---------------------------------------------------------- Ejercicio o Tarea #4 ----------------------------------------------------------
-- Tarea: Implementar fibonacci eficiente en Haskell
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibs!!n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

---------------------------------------------------------- Ejercicio o Tarea #5 ----------------------------------------------------------
-- Tarea: Implementar un reversa eficiente
-- La ineficienca de reversa resulta de tener que agregar el elemento hasta el final en cada llamada recursiva.
revertir :: [a] -> [a]
revertir listaO = recorrer listaO []
   where
   recorrer :: [a] -> [a] -> [a]
   recorrer [] listaR = listaR
   recorrer (x:listaO) listaR = recorrer listaO (x:listaR)

---------------------------------------------------------- Ejercicio o Tarea #6 ----------------------------------------------------------
-- Tarea: Implementar la funcion "zip" de Haskell utilizando pattern matching
miZipPM :: [a] -> [b] -> [(a, b)]
miZipPM [] [] = []
miZipPM _ [] = []
miZipPM [] _ = []
miZipPM (x:xs) (y:ys) = (x,y):miZipPM xs ys

---------------------------------------------------------- Ejercicio o Tarea #7 ----------------------------------------------------------
-- Tarea: definir la funcion "convertirAModulo". Esta funcion debe tomar una lista de enteros y un entero llamado "m" que representa
-- un modulo. Esta funcion debe retornar una nueva lista donde todos los valores de la lista origianl se convierten a su valor 
-- equivalente en el modulo.

convertirAModulo :: Int -> [Int] -> [Int]
convertirAModulo _ [] = []
convertirAModulo n lista = [ x `mod` n | x <- lista]

---------------------------------------------------------- Ejercicio o Tarea #8 ----------------------------------------------------------
-- Tarea: Definir la funcion "sumaModularDeListas". Esta funcion debe aceptar dos listas de enteros y un modulo. Debe producir una nueva
-- lista donde los valores con el mismo indice son sumados entre ellos.
-- Ej. -> sumaModularDeListas [1,2] [4,5] 3 == [mod (1 + 4) 3, mod (2 + 5) 3]

sumaModularDeListas :: [Int] -> [Int] -> Int -> [Int]
sumaModularDeListas [] [] n = []
sumaModularDeListas lista1 lista2 n = [ x `mod` n | x <- zipWith (+) lista1 lista2]
    
---------------------------------------------------------- Ejercicio o Tarea #9 ----------------------------------------------------------
-- Tarea: Definir la funcion "igualdadDeListasModular". Esta funcion acepta dos listas de numeros y un modulo "m". Debe retornar True si 
-- las dos listas son iguales en ese modulo. Ej:
-- igualdadDeListasModular 5 [1,2,3] [6,7,8] == True
-- igualdadDeListasModular 5 [1,2,3] [6,7,9] == False

igualdadDeListasModular :: Int -> [Int] -> [Int] -> Bool
igualdadDeListasModular n [] [] = True
igualdadDeListasModular n lista1 lista2 = convertirAModulo n lista1 == convertirAModulo n lista2

---------------------------------------------------------- Ejercicio o Tarea #10 ----------------------------------------------------------
-- Tarea: Definir la funcion "existeModular". Esta funcion toma un modulo "m", un valor "v" y una lista de valores. Donde retorna true si 
-- la lista tiene un valor equivalente a "v" en el modulo "m". Ej:
-- existeModular 5 8 [1,2,3] == True
-- existeModular 5 8 [16,17,18] == True
-- existeMOdular 5 8 [1,2,9] == False

existeModular :: Integral a => a -> a -> [a] -> Bool
existeModular n v lista = v `elem` ([x `mod` n | x <- lista])

---------------------------------------------------------- Ejercicio o Tarea #11 ----------------------------------------------------------
-- Tarea: Implementar "sum" utilizando fold.
fold :: (estado -> a -> estado) -> estado -> [a] -> estado
fold fCon fNil [] = fNil
fold fCons fNil (x : xs) = fCons (fold fCons fNil xs) x

sumFold :: Num a => [a] -> a
sumFold xs = fold fCons fNil xs
    where
        fCons estado x = estado + x
        fNil = 0

---------------------------------------------------------- Ejercicio o Tarea #12 ----------------------------------------------------------
-- Tarea: Hacer la reduccion de "sum" cuando se implementa mediante un fold.

-- sumFold [1,2]
-- = fold fCons fNil [1,2]
-- | fold: fCons = fCons, fNil = fNil, x = 1, xs = [2]
-- = fCons (fold fCons fNil xs) x [fCons/fCons, fNil/fNil, x/1, xs/2]
-- = fCons (fold fCons fNil [2]) 1
-- | | fold: fCons = fCons, fNil = fNil, x = 2, xs = []
-- = fCons (fCons (fold fCons fNil xs [fCons/fCons, fNil/fNil, x/2, xs/[]]) x ) 1
-- = fCons (fCons (fold fCons fNil []) 2) 1
-- | fold: fCons = fCons, fNil = fNil
-- = fCons (fCons (fNil [fNil/fNil]) 2) 1
-- = fCons (fCons (fNil) 2) 1
-- = fCons (fCons 0 2) 1
-- | fCons: x = 0, y = 2
-- = fCons (x + estado [x/0, y/2]) 1
-- = fCons (0 + estado) 1
-- - fCons estado 1

---------------------------------------------------------- Ejercicio o Tarea #13 ----------------------------------------------------------
-- Tarea: Implementar "reverse" utilizando fold. Puede utilizar la funcion "adjuntar", ya sea en el agregador o en el vacio.

adjuntar' :: a -> [a] -> [a]
adjuntar' a xs = fold agg vacio xs
    where
        agg estado x = x : estado
        vacio = [a]
        
reverseFold :: [a] -> [a]
reverseFold lista = fold agg vacio lista
    where
        agg estado x = adjuntar' x estado
        vacio = []
