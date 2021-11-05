module P4 where

-- Permite incorporar pruebas aleatorias a las propiedades del programa
import Test.QuickCheck

main :: IO ()
main = putStrLn "Hello, Haskell P4!"

-- Enlace de pregunta: https://leetcode.com/problems/permutation-sequence/
-- Enlace de respuesta: https://leetcode.com/problems/permutation-sequence/discuss/1451482/Haskell-Solution
-- Enlace de respuesta adicional resuelta por realizar esta: https://leetcode.com/problems/permutations/discuss/1451495/Haskell-Solution

-- Given n and k, return the kth permutation sequence.

permutaciones :: Int -> Int -> [Int]
permutaciones n k = tomarElemento a
    where
        a = take k (permutaciones' [1..n])

tomarElemento :: [[Int]] -> [Int]
tomarElemento = last

permutaciones' :: [a] -> [[a]]
permutaciones' xs = permu xs []
  where
    permu [] _ = [[]]
    permu [y] ys = (y:) <$> permu ys []
    permu (y : ys) zs = permu [y] (ys ++ zs) ++ permu ys (y : zs)
