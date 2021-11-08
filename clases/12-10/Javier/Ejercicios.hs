module Javier.Ejercicios where 

-- Ejercicio:
-- Leer dos parejas ordenadas de
-- tipo (Float, Float) y calcular
-- la distancia euclideana entre las
-- dos parejas
distancia = do
    punto1 <- readLn :: IO (Float, Float)
    punto2 <- readLn :: IO (Float, Float)
    return (sumaPuntos punto1 punto2)

sumaDistancias listaVertices =
    if length resto > 1 then
        sumaPuntos actual siguiente + sumaDistancias resto
    else
        sumaPuntos actual siguiente
        where
            actual = head listaVertices
            resto = tail listaVertices
            siguiente = head resto

sumaPuntos a b = sqrt ((fst b - fst a)**2 + (snd b - snd a)**2)


-- Ejercicio:
-- Leer parejas ordenadas de float
-- desde la terminal y calcular la
-- distancia de la linea formada
-- por las parejas que fueron leidas
-- de la terminal
longitud = longitudAux []

longitudAux :: [(Float, Float)] -> IO Float
longitudAux lista = do
    punto <- readLn :: IO (Maybe (Float, Float))
    case punto of
        Nothing -> return (sumaDistancias lista)
        Just i' -> do
            longitudAux (i' : lista)