module Main where

import Prelude hiding (sequence)
import Typeclassopedia
import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Monad hiding (sequence)

tree :: Tree String
tree = Node [Node [Node [Leaf "L"], Leaf "R"], Node [Leaf "Alex"], Leaf "Moritz"]

fTree :: Tree (String -> String)
fTree = Node [Leaf id, Leaf reverse]

intTree :: Tree Int
intTree = Node [Leaf 2, Leaf 5]

intTree2 :: Tree Int
intTree2 = Node [Leaf 1, Node [Leaf 10, Leaf 7]]

exMonad :: Tree Int
exMonad = do
    x <- intTree
    y <- intTree2
    if even x then return (x * y)
              else return (x * 2 * y)

exMonadJoin :: Tree Int
exMonadJoin = let treeOfTrees = Node [Leaf intTree, Leaf intTree2]
           in join treeOfTrees

f :: Int -> Maybe Int
f x = if even x then Just (x `div` 2)
                else Nothing

main :: IO ()
main = do
  putStrLn "tree"
  print tree
  putStrLn ""

  putStrLn "intTree"
  print intTree
  putStrLn ""

  putStrLn "intTree2"
  print intTree2
  putStrLn ""

  putStrLn "Functor: fmap length"
  print $ fmap length tree
  putStrLn ""

  putStrLn "Applicative 1: Apply tree of functions"
  print $ fTree <*> tree
  putStrLn ""

  putStrLn "Applicative 2: Multiply values in trees"
  print $ (*) <$> intTree2 <*> intTree
  putStrLn ""

  putStrLn "Monad 1: Double if value in first tree is even"
  print exMonad
  putStrLn ""

  putStrLn "Monad 2: Join tree of trees"
  print exMonadJoin
  putStrLn ""

  putStrLn "Traversable: Traverse intTree with function that may fail"
  print $ traverse f intTree
  putStrLn ""

  putStrLn "Traversable: Sequence list of trees"
  print $ sequence [intTree, intTree2]
  putStrLn ""
