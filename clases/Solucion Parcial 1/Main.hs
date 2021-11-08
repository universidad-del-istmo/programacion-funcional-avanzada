module Main where

esPrimo :: Int -> Bool
esPrimo n = null [x | x <- candidatos, mod n x == 0]
    where
        candidatos = [2 .. div n 2]

primos :: Int -> [Int]
primos n = take n [x | x <- todos, esPrimo x]
    where
        todos = [2 .. ]

miLength xs =
    if null xs
    then 0
    else 1 + miLength (tail xs)

contarPrimos ns = length [x | x <- ns, esPrimo x]

esCercano d punto = sqrt (x*x + y*y) <= d
    where
        x = fst punto
        y = snd punto

puntosCercanos d xs = [x | x <- xs, esCercano d x]

--  4    3
--    
--  1    2
--

esCuadrado xs =
    if length xs /= 4
    then False
    else
        v1x == v4x
        && v1y == v2y
        && v2x == v3x
        && v3y == v4y

    where
        v1 = head xs
        v1x = fst v1
        v1y = snd v1
        v2 = head (tail xs)
        v2x = fst v2
        v2y = snd v2
        v3 = head (tail (tail xs))
        v3x = fst v3
        v3y = snd v3
        v4 = head (tail (tail (tail xs)))
        v4x = fst v4
        v4y = snd v4

miZip xs1 xs2 =
    if null xs1 || null xs2
    then []
    else
        (x1, x2) : miZip xs1' xs2'

    where
        x1 = head xs1
        x2 = head xs2
        xs1' = tail xs1
        xs2' = tail xs2

miSum arg =
    if null arg
    then 0
    else
        x + sum xs'

    where
        x = head arg
        xs' = tail arg

toNumberAux mag cs =
    if null cs
    then 0
    else
        valor * mag + toNumberAux (div mag 10) cs'

    where
        c = head cs
        valor =
            if elem c ['0' .. '9']
            then fromEnum c - fromEnum '0'
            else error "mal valor"
        cs' = tail cs

toNumber cs = toNumberAux mag cs
    where
        mag = 10 ^ (length cs - 1)

split valor xs =
    if null xs
    then ([], [])
    else if x == valor
    then ([], xs')
    else (x : primero, segundo)

    where
        x = head xs
        xs' = tail xs
        resultado = split valor xs'
        primero = fst resultado
        segundo = snd resultado

toNumberDec xs = num + vDec
    where
        xs' = split '.' xs
        num = fromIntegral (toNumber (fst xs'))
        dec = snd xs'
        pDec = length dec
        vDec = fromIntegral (toNumber dec) / fromIntegral (10 ^ pDec)

main :: IO ()
main = putStrLn "Hello, Haskell!"
