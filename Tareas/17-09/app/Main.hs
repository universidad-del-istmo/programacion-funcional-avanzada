{-# LANGUAGE NoImplicitPrelude #-}

module Main where


import Prelude ( Read(..), Show(..), read, undefined, Int, Show, (-),(+), undefined, Enum (succ) )

data Lista = Nil | Cons Int Lista deriving Show

enumerar 0 = Nil
enumerar n = Cons n (enumerar (n - 1))

pushBack x Nil = Cons x Nil
pushBack x (Cons a as) = Cons a (pushBack x as)

map :: (Int -> Int) -> Lista -> Lista
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)
-- map (+1) (Cons 1 (Cons 2 Nil))

reverse Nil = Nil
reverse (Cons x xs) = pushBack x (reverse xs)

-- Ejercicio
-- Definir la funcion "range", esta funcion
-- toma un numero inicial y una cantidad y
-- enumera "cantidad" numeros en una lista
-- empezando por "inicial" e incrementando
-- por uno cada valor 
---prueba range 3 4 
sumar :: Int -> Int -> Lista
sumar i 0 = Nil
sumar i m = Cons (i+(m-1)) (sumar (i) (m - 1))

range :: Int -> Int -> Lista
range ini 0 = Nil
range ini cant = reverse (sumar ini cant)


-- Ejercicio: Definir la funcion "take"
-- esta funcion toma una lista y un
-- numero. Produce una nueva lista donde
-- que tiene los primeros "n" numeros de
-- la lista original.
---prueba take 3 (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 4 (Cons 5 Nil))))))
take :: Int -> Lista -> Lista
take x Nil = Nil
take 0 x = Nil
take n (Cons x xs) = Cons x (take (n-1) xs) 
    


main = undefined
