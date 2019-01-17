{-# LANGUAGE LambdaCase, DeriveFunctor #-}
module Zipper where

import Prelude hiding (reverse)
import qualified Prelude
import Control.Lens hiding (elements)

data Zipper a = Z [a] a [a]
    deriving (Functor)

mkZipper :: Int -> [a] -> Zipper a
mkZipper i l = Z (Prelude.reverse $ take i l) (l !! i) (drop (i+1) l)

elements :: Traversal (Zipper a) (Zipper b) a b
elements f (Z l x r) = Z <$> traverse f (Prelude.reverse l) <*> f x <*> traverse f r

focus :: Lens' (Zipper a) a
focus f (Z l x r) = (\x -> Z l x r) <$> f x

moveLeft :: Zipper a -> Maybe (Zipper a)
moveLeft (Z [] _ _) = Nothing
moveLeft (Z (y:q) x r) = Just $ Z q y (x:r)

moveRight :: Zipper a -> Maybe (Zipper a)
moveRight (Z _ _ []) = Nothing
moveRight (Z l x (y:q)) = Just $ Z (x:l) y q

reverse :: Zipper a -> Zipper a
reverse (Z l x r) = Z (Prelude.reverse r) x (Prelude.reverse l)

insertR :: a -> Zipper a -> Zipper a
insertR y (Z l x r) = Z l x (y:r)
