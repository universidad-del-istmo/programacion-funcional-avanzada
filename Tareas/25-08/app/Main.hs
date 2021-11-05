module Main where

fibonacciEficiente 0 = 0
fibonacciEficiente 1 = 1
fibonacciEficiente n = fibonacci!!n
    where fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)


reverseEficiente [] = []
reverseEficiente lista =  rev lista []
  where
    rev [] n = n
    rev (x:xs) n = rev xs (x:n)

zipPatternM [] [] = []
zipPatternM _ [] = []  
zipPatternM [] _ = []  
zipPatternM (x:xs) (y:ys) = (x,y):zipPatternM xs ys

main :: IO ()
main = putStrLn "Hello, Haskell!"
