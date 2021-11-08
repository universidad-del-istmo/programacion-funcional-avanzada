module Javier.Ejercicios where

data Fraction = Frac Integer Integer deriving Show

-- Ejericio:
-- Fraction es un tipo que representa fracciones.
-- El primer parametro del constructor Frac es
-- el numerador y el segundo es el denominador
-- En otras palabras (4 % 5) o 4/5 es equivalente a Frac 4 5.
-- Su tarea es volver el tipo Fraction una instancia de Num

instance Num Fraction where
    Frac a b + Frac c d = Frac ((div (lcm b d) b * a) + (div (lcm b d) d * c)) (lcm b d)
    Frac a b * Frac c d = Frac (a * c) (b * d)
    abs (Frac a b) = Frac (abs a) (abs b)
    signum = undefined
    fromInteger i = Frac i 1
    Frac a b - Frac c d = Frac ((div (lcm b d) b * a) - (div (lcm b d) d * c)) (lcm b d)
