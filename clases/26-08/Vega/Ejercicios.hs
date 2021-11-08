-- Nombre: Juan Diego Vega

module Vega.Ejercicios where
import Control.Monad (forM, forM_)
import Data.Ratio ((%))
    -- Tarea
-- definir la funcion "convertirAModulo"
-- Esta funcion debe tomar una lista de
-- enteros y un entero llamado "m" que representa
-- un modulo. Esta funcion debe retornar una
-- nueva lista donde todos los valores de la lista
-- origianl se convierten a su valor equivalente
-- en el modulo.
convertirAModulo [] m = []
convertirAModulo (x:xs) m = (mod x m) : (convertirAModulo xs m)
-- Tarea
-- Definir la funcio "sumaModularDeListas"
-- esta funcion debe aceptar dos listas
-- de enteros y un modulo. Debe producir
-- una nueva lista donde los valores con
-- el mismo indice son sumados entre elos.
-- Ej.
-- sumaModularDeListas [1,2] [4,5] 3 == [mod (1 + 2) 3, mod (2 + 5) 3]
sumaModularDeListas [] [] m = []
sumaModularDeListas (x:xs) (y:ys) m = (mod (x + y) m) : (sumaModularDeListas xs ys m)
-- Tarea
-- Definir la funcion "igualdadDeListasModular"
-- Esta funcion acepta dos listas de numeros y
-- un modulo "m". Debe retornar True si las dos
-- listas son iguales en ese modulo
-- igualdadDeListasModular 5 [1,2,3] [6,7,8] == True
-- igualdadDeListasModular 5 [1,2,3] [6,7,9] == False
igualdadDeListasModular m [] [] = True
igualdadDeListasModular m (x:xs) (y:ys) = (mod x m) == (mod y m) && (igualdadDeListasModular m xs ys)
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
existeModular m v [] = False
existeModular m v (x:xs) = ((mod v m) == (mod x m)) || (existeModular m v xs)