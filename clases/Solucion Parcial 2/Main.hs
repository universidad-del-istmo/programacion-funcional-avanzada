module Main where

mcdL :: [Int] -> Int
mcdL [] = 0
mcdL (x : xs) = gcd x (mcdL xs)

mcdL' :: [Int] -> Int
mcdL' xs = foldl agg 0 xs
    where
        agg estado x = gcd x estado

esPrimo n = foldl agg True [2 .. div n 2]
    where
        agg estado x = estado && mod n x /= 0

primos n = take n [x | x <- [2 ..], esPrimo x]

multiplos n k = [x | x <- [k * 2 .. n], mod x k == 0]

contarMultiplos n k = length (multiplos n k)

length' xs = foldl agg 0 xs
    where
        agg estado _ = estado + 1

contarMultiplos' n k = foldl agg 0 [k * 2 .. n]
    where
        agg estado x =
            estado + if mod x k == 0 then 1 else 0

contarFrecuencias n = map gradoMap freqs
    where
        nums = primos n
        m = 1000*n
        freqsMap x = (x, contarMultiplos m x)
        multsDos = fromIntegral (snd (head freqs))
        freqs = map freqsMap nums
        gradoMap (p, ms) = (p, ms, fromIntegral ms / multsDos)

main :: IO ()
main = putStrLn "Hello, Haskell!"
