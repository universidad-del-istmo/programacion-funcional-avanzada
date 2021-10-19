module Main where

leerYSumar = a + b
    where
        a = readLn :: IO Int
        b = readLn :: IO Int

main :: IO ()
main = putStrLn "Hello, Haskell!"
