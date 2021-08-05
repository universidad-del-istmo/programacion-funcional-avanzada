-- Nombre: Elisa Monzon

module Elisa.Ejercicios where

-- y(x) = mx + b

linea m x b y = (m*x) + b

linea_h_1 = linea m x b
    where
        m = 0
        x = 0
        b = 0

linea_v_1 = linea m x b
    where
        m = 1
        x = 1
        b = -3

linea_v_2 = linea m x b
    where
        m = -1
        x = 1
        b = 9


lineaCompuesta x =
    if x == 0 then linea_h_1
    else if x > 1 then linea_v_1
    else linea_v_2


main :: IO ()
main = putStrLn "Hello, Haskell!"