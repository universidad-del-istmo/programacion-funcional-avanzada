-- Nombre: Juan Diego Vega

module Vega.Ejercicios where
data Fraction = MyFrac Integer Integer deriving Show
-- Ejericio:
-- Fraction es un tipo que representa fracciones.
-- El primer parametro del constructor MyFrac es
-- el numerador y el segundo es el denominador
-- En otras palabras (4 % 5) o 4/5 es equivalente a MyFrac 4 5.
-- Su tarea es volver el tipo Fraction una instancia de Num
instance Num Fraction where
    MyFrac alpha beta + MyFrac charly delta = MyFrac ((div (lcm beta delta) beta * alpha) + (div (lcm beta delta) delta * charly)) (lcm beta delta)
    MyFrac alpha beta * MyFrac charly delta = MyFrac (alpha * charly) (beta * delta)
    abs (MyFrac alpha beta) = MyFrac (abs alpha) (abs beta)
    signum = undefined
    fromInteger i = MyFrac i 1
    MyFrac alpha beta - MyFrac charly delta = MyFrac ((div (lcm beta delta) beta * alpha) - (div (lcm beta delta) delta * charly)) (lcm beta delta)