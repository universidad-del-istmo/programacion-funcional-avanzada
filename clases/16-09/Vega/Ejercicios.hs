-- Nombre: Juan Diego Vega
{-# LANGUAGE NoImplicitPrelude #-}
module Vega.Ejercicios where
import Prelude (Read(..), Show(..), read, undefined)
import Data.Ratio ((%))
data Bool = True | False deriving (Read, Show)
data Natural = Cero | Succ Natural deriving Show

Cero + a = a
a + Cero = a
a + (Succ b) = Succ (a + b)
-- Ejercicio: Implementar la multiplicacion
-- de numeros naturales.
Cero * b = Cero
a * Cero = Cero
a * (Succ Cero) = a
a * (Succ (Succ Cero)) = a + a
-- Ejercicio: Implementar el operador
-- "menor que" para numeros naturales.
Cero < _ = False
_ < Cero = True
Succ a < Succ b = a < b
-- Ejercico: Implementar el tipo "semaforo"
-- y la funcion "cambiar" que actualiza
-- el color del semaforo.
data Semaforo = Detente | ReduceVel | Avanza deriving Show

cambiar :: Semaforo -> Semaforo
cambiar Avanza = ReduceVel
cambiar ReduceVel = Detente
cambiar Detente = Avanza
