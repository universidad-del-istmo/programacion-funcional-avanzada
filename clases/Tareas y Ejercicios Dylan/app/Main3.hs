{-# LANGUAGE NoImplicitPrelude #-}

module Main3 where

--main :: IO ()
--main = putStrLn "Hello 3, Haskell!"

import Prelude
    ( Int, Show(..), (+), (-), Enum (succ), undefined )

---------------------------------------------------------- Ejercicio o Tarea #17 ----------------------------------------------------------
-- Ejercicio: Definir la funcion "range", esta funcion toma un numero inicial y una cantidad y enumera "cantidad" numeros en una lista
-- empezando por "inicial" e incrementando por uno cada valor.

data Lista = Nil | Cons Int Lista deriving Show

pushBack x Nil = Cons x Nil
pushBack x (Cons a as) = Cons a (pushBack x as)

reverse Nil = Nil
reverse (Cons x xs) = pushBack x (reverse xs)

enumerar :: Int -> Int -> Lista
enumerar i 0 = Nil
enumerar i c = Cons (i+(c-1)) (enumerar (i) (c - 1))

range :: Int -> Int -> Lista
range ini 0 = Nil
range ini cant = reverse (enumerar ini cant)

---------------------------------------------------------- Ejercicio o Tarea #18 ----------------------------------------------------------
-- Ejercicio: Definir la funcion "take" esta funcion toma un numero y una lista. Produce una nueva lista donde contiene los primeros "n"
-- numeros de la lista original.

take :: Int -> Lista -> Lista
take x Nil = Nil
take 0 x = Nil
take n (Cons x xs) = Cons x (take (n-1) xs)
