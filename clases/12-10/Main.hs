module Main where

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO = (>>=)

bindIOInt :: IO Int -> (Int -> IO Int) -> IO Int
bindIOInt = (>>=)

returnIO :: a -> IO a
returnIO = return

fmapIO :: (a -> b) -> IO a -> IO b
fmapIO = fmap

-- Ejercicio:
-- Leer dos parejas ordenadas de
-- tipo (Float, Float) y calcular
-- la distancia euclideana entre las
-- dos parejas
distancia :: IO Float
distancia = undefined

sumar'' = do
    a <- readLn
    b <- readLn
    return (a + b)

sumar' =
    readLn >>=
        (
            \a -> readLn
                >>=
                    (
                        \b -> return (a + b)
                    )
        )

sumar2 = \a b -> a + b

sumar = readLn `bindIO` bind1 
    where
        bind1 a = readLn `bindIO` (bind2 a)
        bind2 a b = returnIO (a + b)

main :: IO ()
main = putStrLn "Hello, Haskell!"
