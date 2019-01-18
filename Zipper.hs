{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable, DeriveGeneric #-}
module Zipper where

import Prelude hiding (reverse)
import qualified Prelude
import Test.SmallCheck.Series
import GHC.Generics (Generic)
import Control.Lens hiding (elements)

data Zipper a = Z [a] a [a]
    deriving (Functor, Foldable, Generic)

-- SmallCheck
instance Serial m a => Serial m (Zipper a) where
    series = decDepth $ do
        l <- (:) <$> series <~> series
        i <- generate $ \d -> take d [0..(length l - 1)]
        return $ fromList i l

instance Show a => Show (Zipper a) where
    show z = "fromList " ++ show (position z) ++ " " ++ show (toList z)

fromList :: Int -> [a] -> Zipper a
fromList i l = Z (Prelude.reverse $ take i l) (l !! i) (drop (i+1) l)

toList :: Zipper a -> [a]
toList (Z l x r) = Prelude.reverse l ++ [x] ++ r

focus :: Lens' (Zipper a) a
focus f (Z l x r) = (\x -> Z l x r) <$> f x

position :: Zipper a -> Int
position (Z l x r) = length l

moveLeft :: Zipper a -> Maybe (Zipper a)
moveLeft (Z [] _ _) = Nothing
moveLeft (Z (y:q) x r) = Just $ Z q y (x:r)

moveRight :: Zipper a -> Maybe (Zipper a)
moveRight (Z _ _ []) = Nothing
moveRight (Z l x (y:q)) = Just $ Z (x:l) y q

reverse :: Zipper a -> Zipper a
reverse (Z l x r) = Z r x l

-- Insert to the right of focus
insertR :: a -> Zipper a -> Zipper a
insertR y (Z l x r) = Z (x:l) y r

-- Delete focus and move right
deleteR :: Zipper a -> Maybe (Zipper a)
deleteR (Z _ _ []) = Nothing
deleteR (Z l _ (x:q)) = Just $ Z l x q

-- cut everything to the right of the focus
cutR :: Zipper a -> (Maybe (Zipper a), Zipper a)
cutR z@(Z _ _ []) = (Nothing, z)
cutR (Z l x (y:q)) = (Just (Z [] y q), Z l x [])
