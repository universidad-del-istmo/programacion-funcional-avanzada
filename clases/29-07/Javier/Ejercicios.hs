module Javier.Ejercicios where

import Data.Ratio ((%))

import Main
-- Definir una funcion que corresponda
-- a la figura que se presenta a continuacion.
-- Comienza en (0,0)
-- 3 unidades horizontales
-- 3 unidades con pendiente hacia arriba
-- 3 unidades con pendiente hacia abajo
-- 3 unidades mas horizontales
--
--       /\
--      /  \
-- ____/    \____
volcan x
  | x >= 0 && x <= 3 = lineaHorizontal x
  | x > 3 && x <= 6 = lineaAscendente x
  | x > 6 && x <= 9 = lineaDescendente x
  | otherwise = lineaHorizontal x

lineaHorizontal = linea m a b
    where
        m = 0
        a = 0
        b = 0

lineaAscendente = linea m a b
    where
        m = 1 % 0
        a = 0
        b = -3

lineaDescendente = linea m a b
    where
        m = -1 % 1
        a = 0
        b = 9