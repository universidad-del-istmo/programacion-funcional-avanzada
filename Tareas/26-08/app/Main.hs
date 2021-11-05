module Main where


convertirAModulo :: Integral a => a -> [a] -> [a]
convertirAModulo _ [] = []
convertirAModulo n xs = [x `mod` n| x <- xs]

sumaModularDeListas :: Integral a => [a] -> [a] -> a -> [a]
sumaModularDeListas [] [] _ = []
sumaModularDeListas lista1 lista2 n = [ x `mod` n | x <- zipWith (+) lista1 lista2]

igualdadDeListasModular :: Integral a => [a] -> [a] -> a -> Bool
igualdadDeListasModular l1 l2 n = convertirAModulo n l1 == convertirAModulo n l2

existeModular :: Integral a => a -> a -> [a] -> Bool
existeModular n v lista = v `elem` ([x `mod` n | x <- lista])

main :: IO ()
main = putStrLn "Hello, Haskell!"
