module Tree
    (Tree(..)
    ) where

-- TASK 3.2
-- Binary Trees

data Tree a = Branch (Tree a) a (Tree a) | Leaf a
  deriving (Eq, Show)

-- The Foldable instance might prove tricky to define, so
-- defining the specific functions first may be easier!
treeSum :: (Num a) => Tree a -> a
treeSum = foldr (+) 0

treeConcat :: Tree String -> String
treeConcat = foldr (++) []

treeMaximum :: (Ord a) => Tree a -> a
treeMaximum = undefined

-- Write a Foldable instance for Tree.
instance Foldable Tree where
    foldr f b (Leaf a) = f a b
    foldr f b (Branch u a v) = foldr f (foldr f (f a b) u) v
