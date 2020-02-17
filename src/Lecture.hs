--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Applicative functors                                              --
--------------------------------------------------------------------------------

module Lecture where

import Prelude hiding (Applicative(..), Either(..), lookup)

--------------------------------------------------------------------------------
-- Example (Maybe)

lookup :: Eq k => k -> [(k,a)] -> Maybe a
lookup _ [] = Nothing
lookup x ((y,v):ys)
    | x==y      = Just v
    | otherwise = lookup x ys

ex0 :: Maybe String 
ex0 = lookup "name" [("name", "Duckmaster9000"), ("age", "27")]

ex1 :: Maybe String 
ex1 = lookup "address" [("name", "Duckmaster9000"), ("age", "27")]

data Academic = Academic String String String
    deriving Show

infixl 4 `app`
app :: Maybe (a -> b) -> Maybe a -> Maybe b 
app Nothing  _ = Nothing 
app (Just f) x = f <$> x

fromDictionary :: [(String,String)] -> Maybe Academic
fromDictionary dict =
    Academic <$> lookup "name" dict
           `app` lookup "office" dict
           `app` lookup "title" dict

ex2 :: Maybe (String -> String -> Academic)
ex2 = Academic <$> lookup "name" [("name", "Leeky Boi")]

ex3 :: Maybe Academic
ex3 = fromDictionary [("name", "Leeky boi"), ("office", "MB2.31"), ("title", "Lucio Main")]

--------------------------------------------------------------------------------
-- Example (Either)

data Either e a = Left e | Right a
    deriving Show

instance Functor (Either e) where
    fmap _ (Left x)  = Left x
    fmap f (Right y) = Right (f y)

elookup :: (Show k, Eq k) => k -> [(k,a)] -> Either String a
elookup x [] = Left $ "Key " ++ show x ++ " not found!"
elookup x ((y,v):ys)
    | x==y      = Right v
    | otherwise = elookup x ys

ex4 :: Either String String 
ex4 = elookup "address" [("name", "Duckmaster9000"), ("age", "27")]

infixl 4 `eapp`
eapp :: Either e (a -> b) -> Either e a -> Either e b 
eapp (Left err) _ = Left err 
eapp (Right f)  x = f <$> x

fromDictionaryE :: [(String,String)] -> Either String Academic
fromDictionaryE dict =
    Academic <$> elookup "name" dict
          `eapp` elookup "office" dict
          `eapp` elookup "title" dict

ex5 :: Either String Academic 
ex5 = fromDictionaryE [("name", "Leeky boi"), ("office", "MB2.31")]

--------------------------------------------------------------------------------
-- Example (Lists)

appl :: [a -> b] -> [a] -> [b]
appl fs xs = [f x | f <- fs, x <- xs]

ex6 :: [Bool]
ex6 = appl [even, odd] [1,2,3]

--------------------------------------------------------------------------------
