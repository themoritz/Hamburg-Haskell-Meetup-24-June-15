module Main where

import Prelude hiding (sequence)
import Typeclassopedia
import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Monad hiding (sequence)

tree :: Tree String
tree = Node [Node [Node [Leaf "L"], Leaf "R"], Node [Leaf "Single"], Leaf "Mo"]

fTree :: Tree (String -> String)
fTree = Node [Leaf id, Leaf reverse]

intTree :: Tree Int
intTree = Node [Leaf 2, Leaf 5]

intTree2 :: Tree Int
intTree2 = Node [Leaf 1, Leaf 10]

exApplicative :: Tree String
exApplicative = fTree <*> tree

exApplicative2 :: Tree Int
exApplicative2 = (*) <$> intTree2 <*> intTree

exMonad :: Tree Int
exMonad = do
    x <- intTree
    y <- intTree2
    if even x then return (x * y)
              else return (x * 2 * y)

exMonad2 :: Tree Int
exMonad2 = let treeOfTrees = Node [Leaf intTree, Leaf intTree2]
           in join treeOfTrees

exFoldable :: Int
exFoldable = getSum $ foldMap Sum exMonad

f :: Int -> Maybe Int
f x = if even x then Just (x `div` 2)
                else Nothing

main :: IO ()
main = do
  print "intTree"
  print intTree
  print "intTree2"
  print intTree2
  print "exMonad"
  print exMonad
  print "exMonad2"
  print exMonad2
  print "exFoldable"
  print exFoldable
  print "traverse"
  print $ traverse f intTree
  print "sequence"
  print $ sequence [intTree, intTree2]
