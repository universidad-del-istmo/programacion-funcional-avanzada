module Main where

adjuntar x [] = [x]
adjuntar x (y : ys) = x : adjuntar x ys

reversa [] = []
reversa (x : xs) = adjuntar x (reversa xs)

contar [] = 0
contar (_ : xs) = 1 + contar xs

sumar [] = 0
sumar (x : xs) = x + sumar xs

aceder _ [] = error "Lista esta vacia"
aceder 0 (x : _) = x
aceder n (_ : xs) = aceder (n - 1) xs

eliminarEn _ [] = error "Lista esta vacia"
eliminarEn 0 (x : xs) = (x, xs)
eliminarEn n (x : xs) = (e, x : resto)
    where
        (e, resto) = eliminarEn (n - 1) xs

fold :: (estado -> a -> estado) -> estado -> [a] -> estado
fold fCons fNil [] = fNil
fold fCons fNil (x : xs) = fCons (fold fCons fNil xs) x

-- contar' [1,2]
-- = fold agg vacio [1,2]
-- | fold: fCons = agg, fNil = vacio, x = 1, xs = [2]
-- = fCons (fold fCons fNil xs) x [fCons/agg, fNil/vacio, x/1, xs/2]
-- = agg (fold agg vacio [2]) 1 
-- | | fold: fCons = agg, fNil = vacio, x = 2, xs = []
-- = agg (fCons (fold fCons fNil xs [fCons/agg, fNil/vacio, x/2, xs/[]]) x ) 1
-- = agg (agg (fold agg vacio []) 2) 1
-- | fold: fCons = agg, fNil = vacio
-- = agg (agg (fNil [fNil/vacio]) 2) 1
-- = agg (agg (vacio) 2) 1
-- = agg (agg 0 2) 1
-- | agg: estado = 0, x = 2
-- = agg (estado + 1 [estado/0, x/2]) 1
-- = agg (0 + 1) 1
-- - agg 1 1
contar' :: Num estado => [a] -> estado
contar' xs = fold agg vacio xs
    where
        agg estado x = 1 + estado
        vacio = 0

sumar' xs = fold agg vacio xs
    where
        agg estado x = estado + x
        vacio = 0

-- Tarea: implementar "sum" utilizanjdo fold
-- Tarea: Hacer la reduccion de "sum" cuando
-- implementa mediante un fold

adjuntar' a xs = fold agg vacio xs
    where
        agg estado x = x : estado
        vacio = [a]

-- Tarea: implementen "reverse" utilizando fold.
-- Puede utilizar la lfuncion "adjuntar" ya sea
-- en el agregador o en el vacio.

main :: IO ()
main = putStrLn "Hello, Haskell!"
