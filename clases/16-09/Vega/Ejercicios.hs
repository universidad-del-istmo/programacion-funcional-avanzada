-- Nombre: Juan Diego Vega
{-# LANGUAGE NoImplicitPrelude #-}
module Vega.Ejercicios where
import Prelude (Read(..), Show(..), read, undefined)
import Data.Ratio ((%))
-- Ejercicio: Implementar la multiplicacion
-- de numeros naturales.
naturalMult :: Integer -> Integer -> Integer
naturalMult x y = x*y
-- Ejercicio: Implementar el operador
-- "menor que" para numeros naturales.
minuswhat :: Integer -> Integer -> Bool
minuswhat x y = x < y
-- Ejercico: Implementar el tipo "semaforo"
-- y la funcion "cambiar" que actualiza
-- el color del semaforo.
data Semaforo = Detente | ReduceVel | Avanza deriving Show

cambiar :: Semaforo -> Semaforo
cambiar Avanza = ReduceVel
cambiar ReduceVel = Detente
cambiar Detente = Avanza
