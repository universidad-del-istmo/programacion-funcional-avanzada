module P3 where

-- Permite incorporar asociatividad, combinar dos valores cualesquiera de un tipo en uno.
import Data.Semigroup
-- Permite incorporar un monoide, que es un semigrupo con el requisito adicional de un elemento neutro.
import Data.Monoid
-- Permite incorporar pruebas aleatorias a las propiedades del programa
import Test.QuickCheck

main :: IO ()
main = putStrLn "Hello, Haskell P3!"

-- Enlace de pregunta: https://leetcode.com/problems/combinations/
-- Enlace de respuesta: https://leetcode.com/problems/combinations/discuss/1450607/Haskell-Solution

-- Given two integers n and k, return all possible combinations of k numbers out of the range [1, n].

data Comb = Combo {conteo :: Int, combinaciones :: [[Int]]} deriving (Show, Eq, Ord)

instance Semigroup Comb where
    (Combo c1 cs1) <> (Combo c2 cs2) = Combo (c1 + c2) (cs1 ++ cs2)

instance Monoid Comb where
    mempty = Combo 0 []

agregarElemento :: Comb -> Int -> Comb
agregarElemento (Combo c cs) x = Combo c (map (x :) cs)

combinar :: Int -> Int -> Comb
combinar n k | n < 0 || k < 0 = error "Error al `combinar n k`, n y k deben ser numeros naturales"
combinar n k | k == 0 || k == n = Combo 1 [(take k [k-1,k-2..0])]
combinar n k | n < k = mempty
combinar n k = combinar (n-1) k <> (combinar (n-1) (k-1) `agregarElemento` (n-1))
