{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show, (-), undefined)

data Bool = True | False deriving Show

data Natural = Cero | Succ Natural deriving Show

-- O(n)
-- Succ (Succ (Succ Cero)) + Succ (Succ Cero)
-- | n = Succ (Succ (Succ Cero)), m = Succ Cero
-- = Succ (n + m) [n/Succ (Succ (Succ Cero)), m/Succ Cero]
-- = Succ (Succ (Succ (Succ Cero) + Succ Cero)))
-- | n = Succ (Succ (Succ Cero)), m = Cero
-- = Succ (n + m))) [n/Succ (Succ (Succ Cero))] [m/Cero]
-- = Succ (Succ (Succ (Succ (Succ Cero)) + Cero))
-- = Succ (Succ (Succ (Succ (Succ Cero))))
-- Cero + n = n
-- n + Cero = n
-- n + (Succ m) = Succ (n + m)

data Lista = Nil | Cons Int Lista deriving Show

enumerar 0 = Nil
enumerar n = Cons n (enumerar (n - 1))

-- Ejercicio
-- Definir la funcion "range", esta funcion
-- toma un numero inicial y una cantidad y
-- enumera "cantidad" numeros en una lista
-- empezando por "inicial" e incrementando
-- por uno cada valor
range :: Int -> Int -> Lista
range = undefined

main = undefined
