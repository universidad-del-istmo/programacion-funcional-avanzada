module Main where

import Data.Ratio ((%))

id a = a

-- y(x) = mx + b
-- linea :: Float -> Float -> Float -> Float
-- linea :: Int -> Int -> Int -> Int
-- linea :: Num a => a -> a -> a -> a
linea m a b x = m*(x - a) + b

linea1 = linea m a b
    where
        m = 3 % 2
        a = 0
        b = 42

linea2 = linea m a b
    where
        m = 2 % 3
        a = 1
        b = 5

linea3 = linea m a b
    where
        m = 7 % 1
        a = 1
        b = 9

lineaCompuesta x =
    if x >= 0
    then linea1 x
    else if x < 20
    then linea2 x
    else linea3 x

lineaCompuesta' x
    | x >= 0 = linea1 x
    | x < 20 = linea2 x
    | otherwise = linea3 x

puntosLineaCompuesta inicio fin =
    [lineaCompuesta x | x <- [inicio .. fin]]

main :: IO ()
main = putStrLn "Hello, Haskell!"
