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

pushBack x Nil = Cons x Nil
pushBack x (Cons a as) = Cons a (pushBack x as)

reverse Nil = Nil
reverse (Cons x xs) = pushBack x (reverse xs)

-- Ejercicio: Definir la funcion "take"
-- esta funcion toma una lista y un
-- numero. Produce una nueva lista donde
-- que tiene los primeros "n" numeros de
-- la lista original.
take :: Int -> Lista -> Lista
take = undefined

map :: (Int -> Int) -> Lista -> Lista
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

foldl :: (estado -> Int -> estado) -> estado -> Lista -> estado
foldl _ fNil Nil = fNil
foldl fCons fNil (Cons x xs) =
    fCons (foldl fCons fNil xs) x

reverse' xs = foldl acc Nil xs
    where
        acc estado x = pushBack x estado

foldr :: estado -> (Int -> estado -> estado) -> Lista -> estado
foldr fNil fCons xs = aux fNil xs
    where
        aux estado (Cons x xs) = aux (fCons x estado) xs
        aux estado Nil = estado

reverse'' xs = foldr Nil acc xs
    where
        acc a as = Cons a as

main = undefined
