module P2 where

-- Permite incorporar funtores aplicativos
import Control.Applicative
-- Permite incorporar pruebas aleatorias a las propiedades del programa
import Test.QuickCheck

main :: IO ()
main = putStrLn "Hello, Haskell P2!"

-- Enlace de pregunta: https://leetcode.com/problems/generate-parentheses/
-- Enlace de respuesta: https://leetcode.com/problems/generate-parentheses/discuss/1449275/Haskell-Solution-with-Applicative-Functor

-- Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.

combParentesis :: Int -> [String]
combParentesis n = f 0 0 where
    f cantParentesis longitud =
        if cantParentesis < 2*n
            then abrirP <|> cerrarP
        else terminarCP where
            -- Agrega un parentesis de apertura si se puede
            abrirP =
                if longitud < 2*n - cantParentesis
                    then ('(' :) <$> f (cantParentesis+1) (longitud+1)
                else empty

            -- Agrega un parentesis de cierre si se puede
            cerrarP = 
                if longitud > 0
                    then (')' :) <$> f (cantParentesis+1) (longitud-1)
                else empty

            -- Deja de agregar texto. Ya se tiene 2*n caracteres.
            terminarCP = pure ""
