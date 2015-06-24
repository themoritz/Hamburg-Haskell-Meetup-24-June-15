{-# LANGUAGE InstanceSigs, RankNTypes #-}
module Typeclassopedia where

import Data.List (intercalate)
import Control.Applicative
import Data.Foldable hiding (concat)
import Data.Traversable
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
  fmap f (Leaf x)        = Leaf (f x)
  fmap f (Node children) = Node (map (fmap f) children)

instance Applicative Tree where
  pure = Leaf
  Leaf f        <*> x = fmap f x
  Node children <*> x = Node $ treeList children
    where treeList (t:ts) = (t <*> x) : treeList ts
          treeList []     = []

instance Monad Tree where
  return = Leaf
  Leaf x        >>= f = f x
  Node children >>= f = Node $ treeList children
    where treeList (t:ts) = (t >>= f) : treeList ts
          treeList []     = []

instance Foldable Tree where
  foldMap f (Leaf x)      = f x
  foldMap _ (Node [])     = mempty
  foldMap f (Node (c:cs)) = (foldMap f c) `mappend` (foldMap f (Node cs))

instance Traversable Tree where
  traverse m (Leaf x)        = Leaf <$> m x
  traverse m (Node children) = Node <$> treeList children
    where treeList (t:ts) = (:) <$> traverse m t <*> treeList ts
          treeList []     = pure []
