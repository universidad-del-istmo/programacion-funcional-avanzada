module Javier.Ejercicios where

-- Tarea
-- definir la funcion "convertirAModulo"
-- Esta funcion debe tomar una lista de
-- enteros y un entero llamado "m" que representa
-- un modulo. Esta funcion debe retornar una
-- nueva lista donde todos los valores de la lista
-- origianl se convierten a su valor equivalente
-- en el modulo.

convertirAModulo lista m =
    if null lista then
        []
    else
        mod elemento m : convertirAModulo resto m
    where
        resto = tail lista
        elemento = head lista

-- Tarea
-- Definir la funcio "sumaModularDeListas"
-- esta funcion debe aceptar dos listas
-- de enteros y un modulo. Debe producir
-- una nueva lista donde los valores con
-- el mismo indice son sumados entre elos.
-- Ej.
-- sumaModularDeListas [1,2] [4,5] 3 == [mod (1 + 2) 3, mod (2 + 5) 3]

sumaModularDeListas lista1 lista2 m =
    if null lista1 || null lista2 then
        []
    else
        mod (elemento1 + elemento2) : sumaModularDeListas resto1 resto2 m
    where
        elemento1 = head lista1
        elemento2 = head lista2
        resto1 = tail lista1
        resto2 = tail lista2

-- Tarea
-- Definir la funcion "igualdadDeListasModular"
-- Esta funcion acepta dos listas de numeros y
-- un modulo "m". Debe retornar True si las dos
-- listas son iguales en ese modulo
-- igualdadDeListasModular 5 [1,2,3] [6,7,8] == True
-- igualdadDeListasModular 5 [1,2,3] [6,7,9] == False

igualdadDeListasModular lista1 lista2 m
  | null lista1 || null lista2 =
    True
  | mod elemento1 m == mod elemento2 m =
    igualdadDeListasModular resto1 resto2 m
  | otherwise =
    False
  where
      elemento1 = head lista1
      elemento2 = head lista2
      resto1 = tail lista1
      resto2 = tail lista2

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

existeModular m v lista
  | null lista =
    False
  | mod elemento m == mod v m =
    True
  | otherwise =
    existeModular m v resto
  where
      elemento = head lista
      resto = tail lista