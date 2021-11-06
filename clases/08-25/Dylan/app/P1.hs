module P1 where

-- Permite incorporar pruebas aleatorias a las propiedades del programa
import Test.QuickCheck

main :: IO ()
main = putStrLn "Hello, Haskell P1!"

-- Enlace de pregunta: https://leetcode.com/problems/integer-to-roman/
-- Enlace de respuesta: https://leetcode.com/problems/integer-to-roman/discuss/1449155/Haskell-Solution

-- Given an integer, convert it to a roman numeral.

mapeo = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"), (90,"XC"), (50,"L"), 
            (40,"XL"), (10,"X"), (9,"IX"), (5,"V"), (4,"IV"), (1,"I")]

-- Caso especial del 0
numRomano :: Int -> String
numRomano 0 = "N"
numRomano x = numRomano' x

numRomano' :: Int -> String
numRomano' x 
  | x == 0 = ""
  | x > 0 = b ++ numRomano' (x - a)
      where (a, b) = head $ filter (\(a,_) -> a <= x) mapeo
