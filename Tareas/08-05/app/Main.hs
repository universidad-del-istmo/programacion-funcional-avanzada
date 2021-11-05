module Main where

miTake :: Int -> [a] -> [a]
miTake _ [] = []
miTake 0 _ = []
miTake n (x:lista) = x:miTake (n-1) lista

miDrop :: Int -> [a] -> [a]
miDrop _ [] = []
miDrop 0 lista = lista
miDrop n (_:lista) = miDrop (n-1) lista


main :: IO ()
main = putStrLn "Hello, Haskell!"
