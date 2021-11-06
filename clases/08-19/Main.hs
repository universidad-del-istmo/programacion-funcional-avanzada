module Main where

split valor xs =
    if null xs
    then ([], [])
    else if x == valor
    then ([], resto)
    else
        (x : fst resultadoRecursivo, snd resultadoRecursivo)

    where
        x = head xs
        resto = tail xs
        resultadoRecursivo = split valor resto


for estado actualizar xs =
    if null xs
    then estado
    else
        for (actualizar estado x) actualizar xs'
    where
        x = head xs
        xs' = tail xs


splitImperativo valor xs =
    for (False, ([], [])) actualizar xs

    where
        actualizar estado x =
            if x == valor && not bandera
            then (True, (primero, segundo))
            else if bandera
            then (bandera, (primero, segundo ++ [x]))
            else (bandera, (primero ++ [x], segundo))

            where
                bandera = fst estado
                listas = snd estado
                primero = fst listas
                segundo = snd listas

sumaImperativa xs =
    for 0 actualizar xs
    -- for 0 (\i x -> i + x) xs
    where
        actualizar i x = i + x

sumaParejas xs =
    for (0,0) actualizar xs
    where
        actualizar estado x =
            (fst x + fst estado, snd x + snd estado)

contar xs =
    if null xs
    then 0
    else 1 + contar resto

    where
        resto = tail xs

-- contar [1,2,3]
-- = if null [1,2,3] then 0 else 1 + contar resto
-- = if False then 0 else 1 + contar resto
-- = 1 + contar resto
-- = 1 + contar (tail [1,2,3])
-- = 1 + contar [2,3]
-- = 1 + if null [2,3] then 0 else 1 + contar resto
-- ...
-- = 1 + 1 + contar [3]
-- = 1 + 1 + if null [3] then 0 else 1 + contar resto
-- ..
-- = 1 + 1 + 1 + contar []
-- = 1 + 1 + 1 + if null [] then 0 else 1 + contar resto
-- = 1 + 1 + 1 + if True then 0 else 1 + contar resto
-- = 1 + 1 + 1 + 0
-- = 3

factorial n =
    if n == 0
    then 1
    else
        n * factorial (n - 1)

-- factorial 3
-- = if 3 == 0 then 1 else 3 * factorial (3 - 1)
-- = if False then 1 else 3 * factorial (3 - 1)
-- = 3 * factorial (3 - 1)
-- = 3 * factorial 2
-- = 3 * if 2 == 0 then 1 else 2 * factorial (2 - 1)
-- = 3 * 2 * factorial 1
-- = 3 * 2 * if 1 == 0 then 1 else 1 * factorial (1 - 1)
-- = 3 * 2 * 1 * factorial 0
-- = 3 * 2 * 1 * if 0 == 0 then 1 else factorial (0 - 1)
-- = 3 * 2 * 1 * if True then 1 else factorial (0 - 1)
-- = 3 * 2 * 1 * 1
-- = 6

main :: IO ()
main = do
    let mensaje = getLine
    msg <- mensaje
    -- mensaje <- getLine
    putStrLn ("Hola: " ++ msg)
