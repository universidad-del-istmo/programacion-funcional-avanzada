module Main where

import Data.Ratio ((%))

todos :: [Bool] -> Bool
todos [] = True
todos (x : xs) = x && todos xs

sum' :: Num p => [p] -> p
sum' [] = 0
sum' (x : xs) = x + sum xs

data Vector2d = V2 Integer Integer deriving Show

-- (+), (*), abs, signum, fromInteger, (negate | (-))
-- (a, b) (c) 
--        (d)

instance Num Vector2d where
    V2 a b + V2 c d = V2 (a + c) (b + d)
    V2 a b * V2 c d = V2 (a * b + b * c) (a * d + b * c)
    abs (V2 a b) = V2 (abs a) (abs b)
    signum = undefined
    fromInteger i = V2 i i
    V2 a b - V2 c d = V2 (a - c) (b - d)

data Fraction = Frac Integer Integer deriving Show

-- Ejericio:
-- Fraction es un tipo que representa fracciones.
-- El primer parametro del constructor Frac es
-- el numerador y el segundo es el denominador
-- En otras palabras (4 % 5) o 4/5 es equivalente a Frac 4 5.
-- Su tarea es volver el tipo Fraction una instancia de Num

instance Num Fraction where

-- sum'' xs = foldl (+) 0 xs

-- head' :: [Bool] -> Bool
head' (x : _) = x

main :: IO ()
main = putStrLn "Hello, Haskell!"
