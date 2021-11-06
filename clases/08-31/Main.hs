module Main where

-- incrementarTods [1,2,3]
-- | x = 1, xs = [2,3]
-- = x + 1 : incrementarTodos xs [x/1, xs/[2,3]]
-- = 1 + 1 : incrementarTodos [2,3] = 2 : incrementarTodos [2,3]
-- | x = 2, xs = [3]
-- = 2 : x + 1 : incrementarTodos xs [x/2, xs/[3]]
-- = 2 : 2 + 1 : incrementarTodos [3] = 2 : 3 : incrementarTodos [3]
-- | x = 3, xs = []
-- = 2 : 3 : x + 1 : incrementarTodos xs [x/3, xs/[]]
-- = 2 : 3 : 3 + 1 : incrementarTodos [] = 2 : 3 : 4 : incrementarTodos []
-- = 2 : 3 : 4 : []
incrementarTodos :: [Int] -> [Int]
incrementarTodos [] = []
incrementarTodos (x : xs) = x + 1 : incrementarTodos xs

invertirExponente :: [Int] -> [Int]
invertirExponente [] = []
invertirExponente (x : xs) = div 1 x : invertirExponente xs

decrementarTodos :: [Int] -> [Int]
decrementarTodos [] = []
decrementarTodos (x : xs) = x - 1 : decrementarTodos xs


incrementar :: Int -> Int
incrementar x = x + 1

-- incrementarTodos' [1,2,3]
-- | xs = [1,2,3]
-- = mapLista incrementar xs [xs/[1,2,3]]
-- = mapLista incrementar [1,2,3]
-- | mapLista: f = incrementar, x = 1, xs = [2,3]
-- = f x : mapLista f xs [f/incrementar, x/1, xs/[2,3]]
-- = incrementar 1 : mapLista incrementar [2,3]
-- | incrementar: x = 1
-- = (x + 1 [x/1]) : mapLista incrementar [2,3]
-- = (1 + 1) : mapLista incrementar [2,3] = 2 : mapLista incrementar [2,3]
-- | mapLista: f = incrementar, x = 2, xs = [3]
-- = 2 : f x : mapLista f xs [f/incrementar, x/2, xs/[3]]
-- = 2 : incrementar 2 : mapLista incrementar [3]
-- | incrementar: x = 2
-- = 2 : (x + 1[x/2]) : mapLista incrementar [3]
-- = 2 : (2 + 1) : mapLista incrementar [3] = 2 : 3 : mapLista incrementar [3]
-- | mapLista: f = incrementar, x = 3, xs = []
-- = 2 : 3 : f x : mapLista f xs [f/incrementar, x/3, xs/[]]
-- = 2 : 3 : incrementar 3 : mapLista incrementar []
-- | incrementar: x = 3
-- = 2 : 3 : (x + 1[x/3]) : mapLista incrementar []
-- = 2 : 3 : (3 + 1) : mapLista incrementar [] = 2 : 3 : 4 : mapLista incrementar []
-- = 2 : 3 : 4 : []
incrementarTodos' xs = mapLista incrementar xs

decrementar x = x - 1

decrementarTodos' xs = mapLista decrementar xs

mapLista f [] = []
mapLista f (x : xs) = f x : mapLista f xs

aumentarBase :: Functor f => f Int -> f Int
aumentarBase xs = fmap funcion xs
    where
        funcion x = x * 10

divSafe x 0 = Nothing
divSafe x y = Just (div x y)

main :: IO ()
main = putStrLn "Hello, Haskell!"
