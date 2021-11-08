{-# LANGUAGE FlexibleContexts #-}
module Javier.Ejercicios where

import Main

-- Ejercicio:
-- Implementar la funcion "depth" para Json. Esta funcion
-- determina que tan profundo esta el valor mas profundo en
-- un json.
-- Ejemplo:
-- existe'' JProps [("nombre", JString "yo"), ("organizacion", JProps [("nombre", JString "unis"), ("lugar", JArray [JProps ("ciudad", JString "fraijanes")])])] == 3
miDepth :: Json -> Int
miDepth json = undefined 

-- Ejercicio:
-- Escriba la funcion "parseJson" en Haskell que toma
-- un String y produce un valor de tipo Json si el String
-- es un Json valido.
-- Ejemplo:
-- parseJson "{ \"nombre\": \"yo\", \"organizacion\": {\"nombre\": \"unis\", \"lugares\": [{\"ciudad\": \"GT\"}, \"fraijanes\"]}}"
-- == JProps [("nombre",JString "yo"),("organizacion",JProps [("nombre",JString "unis"),("lugares",JArray [JProps [("ciudad",JString "GT")],JString "fraijanes"])])]

parseJson json = undefined 