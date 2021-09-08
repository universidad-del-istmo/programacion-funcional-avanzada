module Main where

sumarDos a b = a + b

mult a 0 = 0
mult a b = sumarDos a a + mult a (b - 1)

pushBack a [] = [a]
pushBack a (x : xs) = x : pushBack a xs

reversa [] = []
reversa (x : xs) = pushBack x (reversa xs) 


-- digitosANumero [1,2,3] == 123
-- observacion: 123 = 3 * 1 + 2 * 10 + 1 * 100
digitosANumero xs = aux 1 xs'
    where
        xs' = reversa xs
        aux _ [] = 0
        aux p (n : ns) = p * n + aux (p * 10) ns

digitosANumero' xs = res
    where
        (res, _) = aux pInicial xs
        pInicial = 10 ^ (length xs - 1)
        -- Utilizar foldl para definir aux
        aux p ns = foldl acumulador (0, p) ns
        acumulador (resultado, p) x = (x * p + resultado, div p 10) 

mcd a b = aux (1,1)
    where
        aux (res, i)
            | i == a || i == b = res
            | mod a i == 0 && mod b i == 0 =
                aux (i, i + 1)
            | otherwise = aux (res, i + 1)

mcd' a b =
    -- head [i | i <- candidatos, mod a i == 0, mod b i == 0]
    foldl acumulador 1 candidatos
    where
        candidatos = reverse [1 .. min a b]
        acumulador res i
            | i > res && mod a i == 0 && mod b i == 0 = i
            | otherwise = res

mcm a b =
    aux (a * b, a * b)
        where
            aux (res, i)
                | i < a || i < b = res
                | mod i a == 0 && mod i b == 0 = aux (i, i - 1)
                | otherwise = aux (res, i - 1)

main :: IO ()
main = putStrLn "Hello, Haskell!"
