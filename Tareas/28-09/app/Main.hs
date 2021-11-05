module Main where
    
data List a = Cons a (List a) | Nil deriving (Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

incrementar :: (Functor f, Num b) => f b -> f b
incrementar valores = fmap (+1) valores

data IList = ICons Int IList | INil deriving Show

-- Esto no es perimitido por que Functor
-- requiere un tipo con un parametro
-- generico
-- instance Functor IList where
--    fmap f INil = INil
--    fmap f (ICons x xs) = ICons (f x) (fmap f xs)

data Vector2d a = V2 a a deriving Show

instance Functor Vector2d where
    fmap f (V2 a b) = V2 (f a) (f b)

data ArbolBinario a =
    Rama a (ArbolBinario a) (ArbolBinario a)
    | Hoja a
    | Nada
    deriving Show

--    3
--   /
-- 1     4
--   \  /
--     2   6
--      \ / 
--       5
--
arbolEjemplo = Rama 1 (Hoja 3) (Rama 2 (Hoja 4) (Rama 5 (Hoja 6) Nada))

-- Ejercicio:
-- Defina la instancia de Functor (solo fmap)
-- para el ArbolBinario

instance Functor ArbolBinario where
    fmap x Nada = Nada
    fmap f (Rama a b c) = Rama (f a) (fmap f b) (fmap f c)
    fmap f (Hoja a) = Hoja (f a)

main :: IO ()
main = putStrLn "Hello, Haskell!"
