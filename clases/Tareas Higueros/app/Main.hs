-- {-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Data.Ratio ((%))
import Control.Monad (forM, forM_)
-- import Prelude (Read(..), Show(..), read, undefined, Int, IO, Integer, Num, putStrLn, mod, (==), (&&), (+), (-), Bool(True), Bool(False), (||), foldl, (++), map, (*), (<), (>))

main :: IO ()
main = putStrLn "Hello, Haskell!"


-- 4 tareas - 26-08-2021

-- definir la funcion "convertirAModulo"
-- Esta funcion debe tomar una lista de
-- enteros y un entero llamado "m" que representa
-- un modulo. Esta funcion debe retornar una
-- nueva lista donde todos los valores de la lista
-- origianl se convierten a su valor equivalente
-- en el modulo.

convertirAModulo [] m = []
convertirAModulo (x:xs) m = (mod x m) : (convertirAModulo xs m)

-- Definir la funcio "sumaModularDeListas"
-- esta funcion debe aceptar dos listas
-- de enteros y un modulo. Debe producir
-- una nueva lista donde los valores con
-- el mismo indice son sumados entre elos.
-- Ej.
-- sumaModularDeListas [1,2] [4,5] 3 == [mod (1 + 2) 3, mod (2 + 5) 3]

sumarModularDeListas [] [] m = []
sumarModularDeListas (x:xs) (y:ys) m = (mod (x + y) m) : (sumarModularDeListas xs ys m)

-- Definir la funcion "igualdadDeListasModular"
-- Esta funcion acepta dos listas de numeros y
-- un modulo "m". Debe retornar True si las dos
-- listas son iguales en ese modulo
-- igualdadDeListasModular 5 [1,2,3] [6,7,8] == True
-- igualdadDeListasModular 5 [1,2,3] [6,7,9] == False

igualdadDeListasModular m [] [] = True
igualdadDeListasModular m (x:xs) (y:ys) = (mod x m) == (mod y m) && (igualdadDeListasModular m xs ys)

-- Definir la funcion "existeModular"
-- Esta funcion toma un modulo "m", un valor "v"
-- y una lista de valores. Retorna true si la
-- lista tiene un valor equivalente a "v" en el
-- modulo "m"
-- Ej:
-- existeModular 5 8 [1,2,3] == True
-- existeModular 5 8 [16,17,18] == True
-- existeMOdular 5 8 [1,2,9] == False

existeModular m v [] = False
existeModular m v (x:xs) = ((mod v m) == (mod x m)) || (existeModular m v xs)

-- 3 tareas - 25-08-2021

-- Tarea: implementar fibonacci eficiente en Haskell

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Tarea: Implemente un reversa eficiente
-- La ineficienca de reversa resulta de
-- tener que agregar el elemento hasta
-- el final en cada llamada recursiva.

reversaEficiente [] = []
reversaEficiente (x:xs) = (reversaEficiente xs) ++ [x]

-- Tarea: implementar la funcion "zip" de Haskell
-- utilizando pattern matching

zipImpl [] [] = []
zipImpl (x:xs) (y:ys) = (x,y) : zipImpl xs ys

-- 3 tareas - 02-09-2021

-- Tarea: implementar "sum" utilizanjdo fold
-- Tarea: Hacer la reduccion de "sum" cuando
-- implementa mediante un fold

sumFold [] = 0
sumFold (x:xs) = x + foldl (+) 0 xs

-- Tarea: implementen "reverse" utilizando fold.
-- Puede utilizar la lfuncion "adjuntar" ya sea
-- en el agregador o en el vacio.

reverseFold [] = []
reverseFold (x:xs) = foldl adjuntar [] (x:xs)

adjuntar [] x = [x]
adjuntar (y:ys) x = x : (y:ys)

-- 1 tareas - 24-09-2021

-- Fraction es un tipo que representa fracciones.
-- El primer parametro del constructor Frac es
-- el numerador y el segundo es el denominador
-- En otras palabras (4 % 5) o 4/5 es equivalente a Frac 4 5.
-- Su tarea es volver el tipo Fraction una instancia de Num

-- data Fraction = Frac Integer Integer deriving Show

-- instance {-# OVERLAPS #-} Num Fraction where

-- fractionImpl numerador denominador = Frac numerador denominador

-- 3 ejerci - 16-09-2021

-- data Natural = Cero | Succ Natural deriving Show

-- Ejercicio: Implementar la multiplicacion
-- de numeros naturales.

-- Cero + a = a
-- a + Cero = a
-- a + (Succ b) = Succ (a + b)

-- Ejercicio: Implementar el operador
-- "menor que" para numeros naturales.

-- Cero < _ = False 
-- _ < Cero = False
-- (Succ a) < (Succ b) = a < b

-- 2 ejerci - 30-09-2021

-- 2 ejerci - 17-09-2021

-- Ejercicio
-- Definir la funcion "range", esta funcion
-- toma un numero inicial y una cantidad y
-- enumera "cantidad" numeros en una lista
-- empezando por "inicial" e incrementando
-- por uno cada valor

range inicial cantidad =
  if cantidad == 0
    then []
    else inicial : (range (inicial + 1) (cantidad - 1))

-- Ejercicio: Definir la funcion "take"
-- esta funcion toma una lista y un
-- numero. Produce una nueva lista donde
-- que tiene los primeros "n" numeros de
-- la lista original.
takeImpl [] _ = []
takeImpl (x:xs) 0 = []
takeImpl (x:xs) n = x : (takeImpl xs (n - 1))

-- 2 ejerci - 08-05-2021

-- Dar una definicion propioa de la
-- funcion take

miTake n lista = takeImpl lista n

-- Dar una definicion propioa de la
-- funcion drop

dropImpl [] _ = []
dropImpl (x:xs) 0 = (x:xs)
dropImpl (x:xs) n = dropImpl xs (n - 1)

miDrop n lista = dropImpl lista n

-- 2 ejerci - 23-09-2021
-- 1 ejerci - 28-09-2021

-- Ejercicio:
-- Defina la instancia de Functor (solo fmap)
-- para el ArbolBinario

data Vector2d a = V2 a a deriving Show

instance Functor Vector2d where
    fmap f (V2 a b) = V2 (f a) (f b)

data ArbolBinario a =
    Rama a (ArbolBinario a) (ArbolBinario a)
    | Hoja a
    | Nada
    deriving Show

instance Functor ArbolBinario where
    fmap f Nada = Nada
    fmap f (Hoja x) = Hoja (f x)
    fmap f (Rama x izq der) = Rama (f x) (fmap f izq) (fmap f der)

-- 2 ejerci - 12-10-2021

-- Ejercicio:
-- Leer dos parejas ordenadas de
-- tipo (Float, Float) y calcular
-- la distancia euclideana entre las
-- dos parejas
distancia :: IO Float
distancia = do
    putStrLn "Ingrese la primera pareja de numeros -> X1"
    x1 <- readLn
    putStrLn "Ingrese la primera pareja de numeros -> Y1"
    y1 <- readLn
    putStrLn "Ingrese la segunda pareja de numeros -> X2"
    x2 <- readLn
    putStrLn "Ingrese la segunda pareja de numeros -> Y2"
    y2 <- readLn
    return (sqrt ((x1 - x2)^2 + (y1 - y2)^2))

-- Ejercicio:
-- Leer parejas ordenadas de float
-- desde la terminal y calcular la
-- distancia de la linea formada
-- por las parejas que fueron leidas
-- de la terminal
longitud :: IO Float
longitud = do
    putStrLn "ingresar un arreglo de parejas ordenas"
    xs <- readLn
    return (sqrt (sum (map (\(x, y) -> (x - y)^2) xs)))

-- 1 ejerci - 14-10-2021

-- Ejercicio: Proveer una implementacion propia
-- de la funcion "forM"
forM' :: [a] -> (a -> IO b) -> IO [b]
forM' [] _ = return []
forM' (x:xs) f = do
    y <- f x
    ys <- forM' xs f
    return (y:ys)

-- 1 ejerci - 15-10-2021
-- 1 ejerci - 29-07-2021
-- Definir una funcion que corresponda
-- a la figura que se presenta a continuacion.
-- Comienza en (0,0)
-- 3 unidades horizontales
-- 3 unidades con pendiente hacia arriba
-- 3 unidades con pendiente hacia abajo
-- 3 unidades mas horizontales
--
--       /\
--      /  \
-- ____/    \____
volcan :: [(Float, Float)]
volcan =
    [ (0, 0)
    , (3, 0)
    , (6, 3)
    , (9, 3)
    , (12, 0)
    , (15, 0)
    ]

printVolcan :: [(Float, Float)] -> IO ()
printVolcan [] = return ()
printVolcan (x:xs) = do
    putStrLn ("(" ++ show (fst x) ++ "," ++ show (snd x) ++ ")")
    printVolcan xs

-- 1 ejerci clase 02-08-2021

contar' xs = foldl fCons fNil xs
    where
        fCons estado x = (estado + 1) + fNil
        fNil = 0

-- Tarea uno
sumar xs = foldl fCons fNil xs
    where
        fCons estado x = (estado + x) + fNil
        fNil = 0

-- Tarea dos
adjuntar' a xs = foldl fCons fNil xs
    where
        fCons estado x = x: estado
        fNil = [a]
reversa' xs = foldl fCons fNil xs
    where
        fCons estado x =  adjuntar' x estado
        fNil = []

digitosANumeros' xs = foldl 1 xs'
    where
        xs' = reverse xs
        foldl estado [] = 0
        fCons estado x = (estado * 10) + x




