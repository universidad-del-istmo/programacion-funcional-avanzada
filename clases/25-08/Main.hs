module Main where

factorialConIf n =
    if n == 0
    then 1
    else n * factorialConIf (n - 1)

-- factorial 3
-- | n = 3
-- = n * factorial (n - 1) [n/3]
-- = 3 * factorial (3 - 1)
-- = 3 * factorial 2
-- | n = 2
-- = 3 * n * factorial (n - 1) [n/2]
-- = 3 * 2 * factorial (2 - 1)
-- = 3 * 2 * factorial 1
-- | n = 1
-- = 3 * 2 * n * factorial (n - 1) [n/1]
-- = 3 * 2 * 1 * factorial (1 - 1)
-- = 3 * 2 * 1 * factorial 0
-- = 3 * 2 * 1 * 1
-- = 6
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialCase n =
    case n of
        0 -> 1
        n' -> n' * factorial (n' - 1)

-- fibonacci 4
-- | n = 4
-- = fibonacci (n - 1) + fibonacci (n - 2) [n/4]
-- = fibonacci (4 - 1) + fibonacci (4 - 2)
-- = fibonacci 3 + fibonacci 2
-- | n = 3
-- = (fibonacci (n - 1) + fibonacci (n - 2)) + fibonacci 2 [n/3]
-- = (fibonacci (3 - 1) + fibonacci (3 - 2)) + fibonacci 2
-- = (fibonacci 2 + fibonacci 1) + fibonacci 2
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Tarea: implementar fibonacci eficiente en Haskell
-- https://www.geeksforgeeks.org/program-for-nth-fibonacci-number/

range 1 = [1]
range n = n : range (n - 1)

agregarAlFinal x [] = [x]
agregarAlFinal x (n : ns) =
    n : agregarAlFinal x ns

miReversa [] = []
miReversa (x : xs) = agregarAlFinal x (miReversa xs)

-- Tarea: Implemente un reversa eficiente
-- La ineficienca de reversa resulta de
-- tener que agregar el elemento hasta
-- el final en cada llamada recursiva.

miHead (x : _) = x
miHead _ = error "Head solo lista llena"

-- [1,2,3,4] -> [2,1,4,3]
voltearParejas [] = []
voltearParejas (x1 : x2 : resto) =
    x2 : x1 : voltearParejas resto
voltearParejas [a] = [a]

voltearTripletas [] = []
voltearTripletas (x1 : x2 : x3 : resto) =
    x3 : x2 : x1 : voltearTripletas resto
voltearTripletas xs = xs

-- Tarea: implementar la funcion "zip" de Haskell
-- utilizando pattern matching

main :: IO ()
main = putStrLn "Hello, Haskell!"
