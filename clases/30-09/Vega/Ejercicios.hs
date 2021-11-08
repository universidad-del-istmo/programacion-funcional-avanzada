-- Nombre: Juan Diego Vega

module Vega.Ejercicios where

-- Ejercicio:
-- Defina una instancia de "ToJson" para los tipos
-- Foldable de parejas ordenadas, dodnde el primer
-- miembro de la pareja es un String, y el segundo es
-- un tipo que puede convertirsae a Json.
-- Esto debe ser convertido a un objeto con keys/values
-- que utiliza el constructor "JObject"
-- toJson [("cantidad", 5 :: Int), ("tiempo": 4 :: Int)] -> JObject [("cantidad", JNumber 5), ("tiempo", JNumber 4)]


-- Ejercicio
-- Definan un typeclass llamada "Diccionario",
-- este typeclass es para tipos de valores
-- que tienen valores internos que pueden ser
-- accesados por una palabra como indice.
-- Tambien crear tres instancias de este typeclass:
-- (1) [(String, a)]
-- (2) Json, solo cuando es un JObject, retornar nothing si
--     no es un JObject
-- (3) Para todo objeto que se puede convertir a Json
-- get "key" [("a", 5), ("b", 6), ("key", 7)] => Just 7
-- get "noExiste" [("a", 5), ("b", 6), ("key", 7)] => Nothing