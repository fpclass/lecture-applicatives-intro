--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 15: Functors & Applicatives                                        --
--------------------------------------------------------------------------------

module Lecture15 where

import Prelude hiding (Functor(..), Applicative(..), ($), (<$>), lookup)

--------------------------------------------------------------------------------
-- Preliminaries

infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

--------------------------------------------------------------------------------
-- Functors

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- | Operator alias for `fmap` which associates to the left with precedence 4.
infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- | Functor instance for lists.
instance Functor [] where
    fmap _ []     = []
    fmap f (x:xs) = f x : map f xs

-- | Functor instance for Maybe.
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

e0 :: Maybe Int
e0 = (+5) <$> safediv 8 4

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)

instance Functor BinTree where
    fmap f (Leaf x)   = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

--------------------------------------------------------------------------------
-- Applicatives

infixl 4 <*>
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    pure = Just

    (Just f) <*> (Just x) = Just (f x)
    _        <*> _        = Nothing


instance Applicative [] where
    pure x = [x]

    fs <*> xs = [f x | f <- fs, x <- xs]

--------------------------------------------------------------------------------
-- Example

lookup :: Eq k => k -> [(k,a)] -> Maybe a
lookup _ [] = Nothing
lookup x ((y,v):ys)
    | x==y      = Just v
    | otherwise = lookup x ys

data Academic = Academic String String String
    deriving Show

fromRequestParams :: [(String,String)] -> Maybe Academic
fromRequestParams req =
    Academic <$> lookup "name" req
             <*> lookup "room" req
             <*> lookup "witter" req

--------------------------------------------------------------------------------
