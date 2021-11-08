-- Nombre: Juan Diego Vega

module Vega.Ejercicios where


-- Ejercicio:
-- Leer dos parejas ordenadas de
-- tipo (Float, Float) y calcular
-- la distancia euclideana entre las
-- dos parejas
totalPts a b = sqrt ((fst b - fst a) ** 2 + (snd b - snd a) ** 2)

totalDists vertices = 
    if length sobrante > 1 then 
        totalPts ahora (head sobrante) + totalDists sobrante
    else
        totalPts ahora (head sobrante)
        where
            ahora = head vertices
            sobrante = tail vertices

-- Ejercicio:
-- Leer parejas ordenadas de float
-- desde la terminal y calcular la
-- distancia de la linea formada
-- por las parejas que fueron leidas
-- de la terminal

