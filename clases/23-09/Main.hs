module Main where

data List a = Nil | Cons a (List a) deriving Show

foldl' :: (s -> a -> s) -> s -> List a -> s
foldl' _ fNil Nil = fNil
foldl' fCons fNil (Cons x xs) = fCons (foldl' fCons fNil xs) x

map' f xs = foldl' fCons Nil xs
    where
        fCons s x = Cons (f x) s

-- { "nombre": "yo", "organizacion": {"nombre": "unis", "lugares": [{"ciudad": "GT"}, "fraijanes"]}}
-- JProps [("nombre", JString "yo"), ("organizacion", JProps [("nombre", JString "unis"), ("lugar", JString "fraijanes")])]

data Json =
    JString String
    | JNumber Int
    | JBool Bool
    | JNull
    | JArray [Json]
    | JProps [(String, Json)]
    deriving Show

foldJson :: (String -> s) -> (Int -> s) -> (Bool -> s) -> s -> ([s] -> s) -> ([(String, s)] -> s) -> Json -> s
foldJson fString fInt fBool fNull fArray fProps (JString s) = fString s
foldJson fString fInt fBool fNull fArray fProps (JNumber i) = fInt i
foldJson fString fInt fBool fNull fArray fProps (JBool b) = fBool b 
foldJson fString fInt fBool fNull fArray fProps JNull = fNull
foldJson fString fInt fBool fNull fArray fProps (JArray js) =
    fArray (map (foldJson fString fInt fBool fNull fArray fProps) js)
foldJson fString fInt fBool fNull fArray fProps (JProps js) =
    fProps (map jPropsMap js)
    where
        jPropsMap (key, json) = (key, foldJson fString fInt fBool fNull fArray fProps json)

existe :: String -> Json -> Bool
existe query (JString valor) = query == valor
existe _ (JBool _) = False
existe _ JNull = False
existe _ (JNumber _) = False
existe query (JArray js) = any (existe query) js
existe query (JProps xs) = any buscarString xs
    where
        buscarString (key, value) = key == query || existe query value

existeJson :: String -> Json -> Bool
existeJson query json = foldJson fString fInt fBool fNull fArray fProps json
    where
        fString s = s == query
        fInt _ = False
        fBool _ = False
        fNull = False
        fArray = any id
        fPropsAny (key, b) = key == query || b
        fProps = any fPropsAny 

-- Version de existe para List
existe' :: String -> List String -> Bool
existe' query Nil = False
existe' query (Cons x xs) = query == x || existe' query xs

-- Version de existe utilizando foldl'
existe'' query xs = foldl' fCons False xs
    where
        fCons s x = x == query || s

-- Ejercicio:
-- Implementar la funcion "depth" para Json. Esta funcion
-- determina que tan profundo esta el valor mas profundo en
-- un json.
-- Ejemplo:
-- existe'' JProps [("nombre", JString "yo"), ("organizacion", JProps [("nombre", JString "unis"), ("lugar", JProps [("ciudad", JString "fraijanes")])])] == 3
depht = undefined

-- Ejercicio:
-- Escriba la funcion "parseJson" en Haskell que toma
-- un String y produce un valor de tipo Json si el String
-- es un Json valido.
-- Ejemplo:
-- parseJson "{ \"nombre\": \"yo\", \"organizacion\": {\"nombre\": \"unis\", \"lugares\": [{\"ciudad\": \"GT\"}, \"fraijanes\"]}}"
-- == JProps [("nombre",JString "yo"),("organizacion",JProps [("nombre",JString "unis"),("lugares",JArray [JProps [("ciudad",JString "GT")],JString "fraijanes"])])]

parseJson = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
