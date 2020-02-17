--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Applicative functors                                              --
--------------------------------------------------------------------------------

module Lecture where

import Prelude hiding (Applicative(..), lookup)

--------------------------------------------------------------------------------
-- Example

lookup :: Eq k => k -> [(k,a)] -> Maybe a
lookup _ [] = Nothing
lookup x ((y,v):ys)
    | x==y      = Just v
    | otherwise = lookup x ys

data Academic = Academic String String String
    deriving Show

infixl 4 `app`
app :: Maybe (a -> b) -> Maybe a -> Maybe b 
app Nothing  _ = Nothing 
app (Just f) x = f <$> x

fromDictionary :: [(String,String)] -> Maybe Academic
fromDictionary dict =
    Academic <$> lookup "name" dict
           `app` lookup "room" dict
           `app` lookup "title" dict

elookup :: (Show k, Eq k) => k -> [(k,a)] -> Either String a
elookup x [] = Left $ "Key " ++ show x ++ " not found!"
elookup x ((y,v):ys)
    | x==y      = Right v
    | otherwise = elookup x ys

infixl 4 `eapp`
eapp :: Either e (a -> b) -> Either e a -> Either e b 
eapp (Left err) _ = Left err 
eapp (Right f)  x = f <$> x

fromDictionaryE :: [(String,String)] -> Either String Academic
fromDictionaryE dict =
    Academic <$> elookup "name" dict
          `eapp` elookup "room" dict
          `eapp` elookup "title" dict

--------------------------------------------------------------------------------
