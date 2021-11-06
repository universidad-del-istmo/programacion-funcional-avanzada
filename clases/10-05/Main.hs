{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Int (Int8)

class Serializable t where
    serialize :: t -> [Int8]
    deserialize :: [Int8] -> Maybe (t, [Int8])

instance Serializable Int where
    serialize valor = map toEnum bytes
        
        where
            bytes = [
                    mod valor 128,
                    mod (div valor 128) 128,
                    mod (div valor 65536) 128,
                    mod (div valor 16777216) 128
                ]

    deserialize bytes
        | length bytes >= 4 = Just (num, resto)
        | otherwise = Nothing
        where
            num = b1 + (b2 * 128) + (b3 * 65536) + (b4 * 16777216)
            [b1, b2, b3, b4] = map fromEnum $ take 4 bytes
            resto = drop 4 bytes

-- instance Serializable a => Serializable [a] where
--     serialize = concatMap serialize
--     deserialize [] = Nothing
--     deserialize xs =
--         case deserialize xs of
--             Just (valor, resto) ->
--                 case deserialize resto of
--                     Just (otros, sobras) -> Just (valor : otros, sobras)
--                     Nothing -> Just ([valor], [])
--             Nothing -> Nothing


class Unfoldable f where
    unfold :: (s -> Maybe (a, s)) -> s -> f a

instance Unfoldable [] where

    unfold f cero =
        case f cero of
            Nothing -> []
            Just (x, estado) -> x : unfold f estado

range start count =
    unfold desAgg start
    where
        desAgg s
            | s > start + count = Nothing
            | otherwise = Just (s, s + 1)

instance (Foldable f, Unfoldable f, Serializable a) => Serializable (f a) where
    serialize xs = concatMap serialize xs
    deserialize bytes' = Just (unfold desAgg bytes', [])
        where
            desAgg bytes =
                case deserialize bytes of
                    Just (nuevo, resto) -> Just (nuevo, resto)
                    Nothing -> Nothing

main :: IO ()
main = putStrLn "Hello, Haskell!"
