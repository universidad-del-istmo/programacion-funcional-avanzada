module Main where

fold :: (estado -> a -> estado) -> estado -> [a] -> estado
fold fCon fNil [] = fNil
fold fCons fNil (x : xs) = fCons (fold fCons fNil xs) x

sumar' :: Num a => [a] -> a
sumar' xs = fold fCons fNil xs
    where
        fCons estado x = estado + x
        fNil = 0

-----------------------------------------
-- sumFold [1,2]
-- = fold fCons fNil [1,2]
-- | fold: fCons = fCons, fNil = fNil, x = 1, xs = [2]
-- = fCons (fold fCons fNil xs) x [fCons/fCons, fNil/fNil, x/1, xs/2]
-- = fCons (fold fCons fNil [2]) 1
-- | | fold: fCons = fCons, fNil = fNil, x = 2, xs = []
-- = fCons (fCons (fold fCons fNil xs [fCons/fCons, fNil/fNil, x/2, xs/[]]) x ) 1
-- = fCons (fCons (fold fCons fNil []) 2) 1
-- | fold: fCons = fCons, fNil = fNil
-- = fCons (fCons (fNil [fNil/fNil]) 2) 1
-- = fCons (fCons (fNil) 2) 1
-- = fCons (fCons 0 2) 1
-- | fCons: x = 0, y = 2
-- = fCons (x + estado [x/0, y/2]) 1
-- = fCons (0 + estado) 1
-- - fCons estado 1

adjuntar' :: a -> [a] -> [a]
adjuntar' a xs = fold agg vacio xs
    where
        agg estado x = x : estado
        vacio = [a]

reverse' :: [a] -> [a]
reverse' lista = fold agg vacio lista
    where
        agg estado x = adjuntar' x estado
        vacio = []





main :: IO ()
main = putStrLn "Hello, Haskell!"
