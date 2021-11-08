-- Nombre: Juan Diego Vega

module Vega.Ejercicios where
import Data.Ratio ((%))
-- Tarea: implementar fibonacci eficiente en Haskell
-- https://www.geeksforgeeks.org/program-for-nth-fibonacci-number/
miFib :: Integer -> Integer
miFib 0 = 0
miFib 1 = 1
miFib n = miFib (n-1) + miFib (n-2)


-- Tarea: Implemente un reversa eficiente
-- La ineficienca de reversa resulta de
-- tener que agregar el elemento hasta
-- el final en cada llamada recursiva.
miRevert listaA = iteraciones listaA []
   where
   iteraciones :: [a] -> [a] -> [a]
   iteraciones [] listaB = listaB
   iteraciones (x:listaA) listaB = iteraciones listaA (x:listaB)

-- Tarea: implementar la funcion "zip" de Haskell
-- utilizando pattern matching
miZip [] [] = []
miZip (x:xs) (y:ys) = (x,y):miZip xs ys