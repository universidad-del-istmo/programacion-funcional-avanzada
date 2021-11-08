module Javier.Ejercicios where

-- Tarea: implementar fibonacci eficiente en Haskell
-- https://www.geeksforgeeks.org/program-for-nth-fibonacci-number/


miFibonacci n = resultados!!n
    where 
        resultados = 0 : 1 : zipWith (+) resultados resto
        
        resto = tail resultados
-- Tarea: Implemente un reversa eficiente
-- La ineficienca de reversa resulta de
-- tener que agregar el elemento hasta
-- el final en cada llamada recursiva.

reversaEficiente [] = []
reversaEficiente (x : xs) = reversaEficiente xs ++ [x] 


-- Tarea: implementar la funcion "zip" de Haskell
-- utilizando pattern matching

miZip [] _ = []
miZip _ [] = []
miZip lista1 lista2 = 
    (elemento1, elemento2) : miZip resto1 resto2 
    where 
        elemento1 = head lista1
        elemento2 = head lista2
        resto1 = tail lista1
        resto2 = tail lista2