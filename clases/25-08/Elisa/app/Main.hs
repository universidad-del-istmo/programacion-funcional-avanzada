module Main where


--- Pregunta 1 - Find the Duplicate Number - https://leetcode.com/problems/find-the-duplicate-number/

numerosRepetidos :: Eq a => [a] -> [a]
numerosRepetidos x | null x = []
    | head x `elem` tail x = take 1 x ++ numerosRepetidos (filter (/=head x) x)
    | otherwise = numerosRepetidos (tail x)


--- Pregunta 2 - Median of Two Sorted Arrays - https://leetcode.com/problems/median-of-two-sorted-arrays/

promedioArreglos lista1 lista2 = sum lista `div` length lista
    where lista = lista1 ++ lista2

--- Pregunta 3 - Remove duplicate letters - https://leetcode.com/problems/remove-duplicate-letters/ 

eliminarLetraRepetida :: Eq a => [a] -> [a]
eliminarLetraRepetida = stoArray []
    where stoArray letra [] = letra
          stoArray letra (x:xs)
              | x `elem` letra = stoArray letra xs
              | otherwise = stoArray (letra ++ [x]) xs


main :: IO ()
main = putStrLn "Hello, Haskell!"