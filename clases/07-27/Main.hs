module Main where

import Modulo.Programa (duplicar)
import Modulo.Programa2 (triplicar)

sumar5 x = x + 5 

-- main :: IO ()
main = do
    x <- getLine
    putStrLn "Valor duplicado: "
    putStrLn (show (duplicar (read x)))