{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Read(..), Show(..), read, undefined)

data Bool = True | False deriving (Read, Show)

not :: Bool -> Bool
not True = False
not False = True

False && _ = False
_ && False = False
_  && _ = True

True || _ = True
_ || True = True
_ || _ = False

ifThenElse True onTrue _ = onTrue
ifThenElse False _ onFalse = onFalse

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

Cero > _ = False
_ > Cero = True
Succ a > Succ b = a > b

-- Ejercicio: Implementar el operador
-- "menor que" para numeros naturales.
Cero < _ = False
_ < Cero = True
Succ a < Succ b = a > b

-- Ejercico: Implementar el tipo "semaforo"
-- y la funcion "cambiar" que actualiza
-- el color del semaforo.
--verde -> amarillo
-- amarillo -> rojo
data Semaforo = Rojo | Amarillo | Verde deriving Show

cambiar :: Semaforo -> Semaforo
cambiar Verde = Amarillo
cambiar Amarillo = Rojo
cambiar Rojo = Verde


main = undefined
