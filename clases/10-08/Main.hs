module Main where

todosLosNumeros = [1 .. ]

todosLosCuadrados = [x * x | x <- todosLosNumeros ]

esCuadrado n =
    if n == 1
    then True
    else not (null [x | x <- candidatos, x * x == n])
    where
        candidatos = [1 .. div n 2]

todosLosCuadrados' = [x | x <- todosLosNumeros, esCuadrado x ]

reversaAux estado lista =
    if null lista
    then estado
    else
        reversaAux (valor : estado) resto
    where
        valor = head lista
        resto = tail lista

reversa = reversaAux []

main :: IO ()
main = putStrLn "Hello, Haskell!"
