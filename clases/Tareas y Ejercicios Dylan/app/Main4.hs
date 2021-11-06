module Main4 where

import Control.Monad (forM)

main :: IO ()
main = putStrLn "Hello 4, Haskell!"

---------------------------------------------------------- Ejercicio o Tarea #19 ----------------------------------------------------------
-- Ejercicio: Fraction es un tipo que representa fracciones. El primer parametro del constructor Frac es el numerador y el segundo es el 
-- denominador. En otras palabras (4 % 5) o 4/5 es equivalente a Frac 4 5. Su tarea es volver el tipo Fraction una instancia de Num.

data Fraction = Frac Integer Integer deriving Show

instance Num Fraction where
    Frac a b + Frac x y= Frac ((a*y)+(x*b)) (b*y)
    Frac a b * Frac x y= Frac (a*x) (b*y)
    Frac a b - Frac x y= Frac ((a*y)-(x*b)) (b*y)
    abs (Frac a b) = Frac (abs a) (abs b)
    signum (Frac a b) = Frac (signum a) (signum b)
    fromInteger i = Frac i 1

---------------------------------------------------------- Ejercicio o Tarea #20 ----------------------------------------------------------
-- Ejercicio: Defina la instancia de Functor (solo fmap) para el ArbolBinario.

data ArbolBinario a =
    Rama a (ArbolBinario a) (ArbolBinario a)
    | Hoja a
    | Nada
    deriving Show

instance Functor ArbolBinario where
    fmap x Nada = Nada
    fmap f (Rama a b c) = Rama (f a) (fmap f b) (fmap f c)
    fmap f (Hoja a) = Hoja (f a)

---------------------------------------------------------- Ejercicio o Tarea #21 ----------------------------------------------------------
-- Ejercicio: Leer dos parejas ordenadas de tipo (Float, Float) y calcular la distancia euclideana entre las dos parejas.

distancia :: IO Float
distancia =
    readLn >>= bind1
    where
        bind1 a = readLn >>= bind2 a
        bind2 a b = return (euclideana a b)

distancia' :: [(Float,Float)] -> Float
distancia' listaVertices =
    if length resto > 1 then
        euclideana actual siguiente + distancia' resto
    else
        euclideana actual siguiente
        where
            actual = head listaVertices
            resto = tail listaVertices
            siguiente = head resto

euclideana :: Floating a => (a, a) -> (a, a) -> a
euclideana (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)

---------------------------------------------------------- Ejercicio o Tarea #22 ----------------------------------------------------------
-- Ejercicio: Leer parejas ordenadas de float desde la terminal y calcular la distancia de la linea formada por las parejas que fueron 
-- leidas de la terminal.

longitud :: IO Float
longitud = do
    lista <- longitud' []
    return (distancia' lista)

longitud' :: [(Float,Float)] -> IO [(Float,Float)]
longitud' lista = do
    a <- getLine
    if a == ""
        then
            return lista
    else
        longitud' ((read a :: (Float,Float)) : lista)

---------------------------------------------------------- Ejercicio o Tarea #23 ----------------------------------------------------------
-- Ejercicio: Proveer una implementacion propia de la funcion "forM".

forM' :: [a] -> (a -> IO b) -> IO [b]
forM' xs funcion =
    if null xs then
        return []
    else do
        resultado <- funcion valor
        restoResultado <- forM' resto funcion
        return (resultado : restoResultado)
        where
            valor = head xs
            resto = tail xs

---------------------------------------------------------- Ejercicio o Tarea #24 ----------------------------------------------------------
-- Ejercicio: Modifique el tipo "Opcion" tal que el mensaje de error se incluya cuando hay un error en la operacion.

data Opcion a = Hay a | NoHay String deriving Show

instance Functor Opcion where
    fmap f (Hay a) = Hay (f a)
    fmap _ (NoHay a) = NoHay a

instance Applicative Opcion where
    pure = Hay
    (Hay f) <*> (Hay v) = Hay (f v)
    _ <*> _ = NoHay "Error"

instance Monad Opcion where
    return = Hay
    (Hay a) >>= f = f a
    (NoHay "Error") >>= _ = NoHay "Error"

instance MonadFail Opcion where
    fail = NoHay 

safeDiv :: MonadFail m => Int -> Int -> m Int
safeDiv num 0 = fail "Division dentro de cero"
safeDiv num den = return (div num den)

variasDivisiones :: MonadFail m => Int -> Int -> Int -> Int -> m Int
variasDivisiones num d1 d2 d3 = do
    r1 <- safeDiv num d1
    r2 <- safeDiv num d2
    r3 <- safeDiv num d3
    return (r1 + r2 + r3)

variasDivisionesLista :: MonadFail m => Int -> [Int] -> m Int
variasDivisionesLista num dens = do
    resultados <- forM dens (safeDiv num)
    return (sum resultados)
