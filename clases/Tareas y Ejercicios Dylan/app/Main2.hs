{-# LANGUAGE NoImplicitPrelude #-}

module Main2 where

--main :: IO ()
--main = putStrLn "Hello 2, Haskell!"

import Prelude
    ( Read(..), Show(..), read, undefined )

---------------------------------------------------------- Ejercicio o Tarea #14 ----------------------------------------------------------
-- Ejercicio: Implementar la multiplicacion de numeros naturales.

data Natural = Cero | Succ Natural deriving Show

Cero + a = a
a + Cero = a
a + (Succ b) = Succ (a + b)
Cero * b = Cero
a * Cero = Cero
a * (Succ Cero) = a
a * (Succ (Succ Cero)) = a + a

---------------------------------------------------------- Ejercicio o Tarea #15 ----------------------------------------------------------
-- Ejercicio: Implementar el operador "menor que" para numeros naturales.

data Bool = True | False deriving (Read, Show)
Cero < _ = True
_ < Cero = False
Succ a < Succ b = a < b

---------------------------------------------------------- Ejercicio o Tarea #16 ----------------------------------------------------------
-- Ejercicio: Implementar el tipo "semaforo" y la funcion "cambiar" que actualiza el color del semaforo.

data Semaforo = Rojo | Amarillo | Verde deriving Show

cambiar :: Semaforo -> Semaforo
cambiar Verde = Amarillo
cambiar Amarillo = Rojo
cambiar Rojo = Verde
