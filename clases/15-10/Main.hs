module Main where

import Control.Monad (forM)

data Opcion a = Hay a | NoHay deriving Show

instance Functor Opcion where
    fmap f (Hay a) = Hay (f a)
    fmap _ NoHay = NoHay

instance Applicative Opcion where
    pure = Hay
    (Hay f) <*> (Hay v) = Hay (f v)
    _ <*> _ = NoHay

instance Monad Opcion where
    return = Hay
    (Hay a) >>= f = f a
    NoHay >>= _ = NoHay

instance MonadFail Opcion where
    fail _ = NoHay

-- instance Monad Opcion where

    -- Es equivalente a:
    -- return a = Hay a
--    return = Hay

safeDiv :: MonadFail m => Int -> Int -> m Int
safeDiv num 0 = fail "Division dentro de cero"
safeDiv num den = return (div num den)

-- variasDivisiones' num d1 d2 d3 =
--     case safeDiv num d1 of
--         Just r1 ->
--             case safeDiv num d2 of
--                 Just r2 -> case safeDiv num d3 of
--                     Just r3 -> Just (r1 + r2 + r3)
--                     Nothing -> Nothing
--                 Nothing -> Nothing
--         Nothing -> Nothing

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

-- Ejercicio:
-- Modifique el tipo "Opcion" tal que
-- el mensaje de error se incluya cuando
-- hay un error en la operacion.

main :: IO ()
main = putStrLn "Hello, Haskell!"
