module Main where

data Lista a = Nil | Cons a (Lista a) deriving (Eq, Show)

-- data ListaF = NilF | ConsF Float ListaF deriving (Eq, Show)

map' f Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

-- mapf f NilF = NilF
-- mapf f (ConsF x xs) = ConsF (f x) (mapf f xs)

main :: IO ()
main = putStrLn "Hello, Haskell!"
