module Main where

-- Ejericio:
-- Fraction es un tipo que representa fracciones.
-- El primer parametro del constructor Frac es
-- el numerador y el segundo es el denominador
-- En otras palabras (4 % 5) o 4/5 es equivalente a Frac 4 5.
-- Su tarea es volver el tipo Fraction una instancia de Num

data Fraction = Frac Integer Integer deriving Show

instance Num Fraction where
    Frac a b + Frac x y= Frac ((a*y)+(x*b)) (b*y) 
    Frac a b * Frac x y= Frac (a*x) (b*y)
    Frac a b - Frac x y= Frac ((a*y)-(x*b)) (b*y) 
    abs (Frac a b) = Frac (abs a) (abs b)
    signum (Frac a b) = Frac (signum a) (signum b)
    fromInteger i = Frac i 1
    


main :: IO ()
main = putStrLn "Hello, Haskell!"
