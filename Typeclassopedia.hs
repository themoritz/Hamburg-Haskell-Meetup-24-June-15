{-# LANGUAGE InstanceSigs, RankNTypes #-}
module Typeclassopedia where

import Data.List (intercalate)
import Control.Applicative
import Data.Foldable hiding (concat)
import Data.Traversable
import Prelude hiding (sequence)
import Data.Monoid

-- Running example: Tree

data Tree a
  = Leaf a
  | Node [Tree a]

instance Show a => Show (Tree a) where
  show x = intercalate "\n" $ showHelper x

showHelper :: Show a => Tree a -> [String]
showHelper (Leaf x) = [show x]
showHelper (Node children) = "─┐ " : (concat $ map (indent . showHelper) children)
  where indent (x:xs) = (" ├─" ++ x) : map (" │ " ++) xs
        indent [] = []

-- Typeclassopedia instances

instance Functor Tree where
  fmap g (Leaf a) = Leaf (g a)
  fmap g (Node children) = Node $ map (fmap g) children

instance Applicative Tree where
  pure = Leaf
  Leaf g        <*> t = fmap g t
  Node children <*> t = Node $ map (<*> t) children

instance Monad Tree where
  return = pure
  Leaf x        >>= f = f x
  Node children >>= f = Node $ map (>>= f) children

instance Foldable Tree where
  foldMap f (Leaf x)      = f x
  foldMap _ (Node [])     = mempty
  foldMap f (Node (c:cs)) = (foldMap f c) `mappend` (foldMap f (Node cs))

instance Traversable Tree where
  traverse f (Leaf x)        = Leaf <$> f x
  traverse f (Node children) = Node <$> sequenceA (map (traverse f) children)
