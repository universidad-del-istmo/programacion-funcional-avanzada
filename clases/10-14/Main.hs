module Main where

import Control.Monad (forM, forM_)

sumarVarios =
    getLine >>=
        (
            \valorDeConsola ->
                if valorDeConsola == ""
                then return 0
                else
                    sumarVarios >>=
                        (
                            \suma -> return (read valorDeConsola + suma)
                        )
        )

sumarVarios' = do
    valorDeConsola <- getLine
    if valorDeConsola == ""
    then return 0
    else do
        suma <- sumarVarios'
        return (read valorDeConsola + suma)

while :: (Maybe a -> IO Bool) -> (Maybe a -> IO a) -> IO (Maybe a)
while cond accion = while' Nothing
    where
        while' estado = do
            continuar <- cond estado
            if continuar
            then do
                estado' <- accion estado
                while' (Just estado')
            else return estado

sumarVarios''' = while cond accion
    where
        cond Nothing = return True
        cond (Just (_, siguiente)) = return (siguiente /= "")
        accion estado = do
            siguiente <- getLine
            return
                (
                    case estado of
                        Nothing ->  (0, siguiente)
                        Just (sumatoria, previo) -> (sumatoria + read previo, siguiente)
                )

imprimir :: (Show t) => [t] -> IO ()
imprimir valores = forM_ valores (\valor -> putStrLn (show valor))

leerCantidad 0 = return []
leerCantidad n = do
    i <- readLn
    resto <- leerCantidad (n - 1)
    return (i : resto)

leerCantidades :: Read a => [Int] -> IO [[a]]
leerCantidades valores = forM valores leerCantidad

-- Ejercicio: Proveer una implementacion propia
-- de la funcion "forM"
forM' :: [a] -> (a -> IO b) -> IO [b]
forM' = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
