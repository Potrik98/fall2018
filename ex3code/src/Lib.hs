module Lib
    ( listSum
    , listProduct
    , listConcat
    , listMaximum
    , listMinimum
    , sum
    , concat
    , length
    , elem
    , safeMaximum
    , safeMinimum
    , any
    , all
    , foldr
    , Complex(..)
    ) where

import Prelude hiding (foldr, maximum, minimum, any, all, length
                      , concat, sum, product, elem, Foldable(..))

-- TASK 2
-- Bounded parametric polymorphism

-- Implement the following functions that reduce a list to a single
-- value (or Maybe a single value).

-- Maybe is imported from Prelude and is defined like this:
-- data Maybe a = Just a | Nothing

listSum :: (Num a) => [a] -> a
listSum [] = 0
listSum (x:xs) = x + listSum xs

listProduct :: (Num a) => [a] -> a
listProduct [] = 1
listProduct (x:xs) = x * listProduct xs

listConcat :: [[a]] -> [a]
listConcat [] = []
listConcat (x:xs) = x ++ listConcat xs

listMaximum :: (Ord a) => [a] -> Maybe a
listMaximum [] = Nothing
listMaximum (x:xs) = do
    case listMaximum xs of
        Just a -> Just $ max x a
        Nothing -> Just x

listMinimum :: (Ord a) => [a] -> Maybe a
listMinimum [] = Nothing
listMinimum (x:xs) = do
    case listMinimum xs of
        Just a -> Just $ min x a
        Nothing -> Just x

-- TASK 3 Folds

-- TASK 3.1
-- Below our Foldable class is defined. Now define a list instance of
-- Foldable, and then define the Foldable versions of the functions
-- you defined previously (and some more).
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable [] where
  foldr f b [] = b
  foldr f b (x:xs) = f x $ foldr f b xs

--
-- USE FOLDR TO DEFINE THESE FUNCTIONS
--
sum :: (Num a, Foldable t) => t a -> a
sum = foldr (+) 0

concat :: Foldable t => t [a] -> [a]
concat = foldr (++) []

length :: Foldable t => t a -> Int
length = foldr (\a b -> b + 1) 0

elem :: (Eq a, Foldable t) => a -> t a -> Bool
elem e = foldr (\a b -> b || a == e) False

safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum = foldr (\a b -> do
    case b of
        Just c -> Just $ max a c
        Nothing -> Just a) Nothing

safeMinimum :: (Foldable t, Ord a) => t a -> Maybe a
safeMinimum = foldr (\a b -> do
    case b of
        Just c -> Just $ min a c
        Nothing -> Just a) Nothing
-- The functions "any" and "all" check if any or all elements of a
-- Foldable satisfy the given predicate.
--
-- USE FOLDR
--
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = foldr (\a b -> b || p a) False

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = foldr (\a b -> b && p a) True

-- TASK 4
-- Num Complex
 
data Complex = Complex Double Double deriving (Eq) 
 
instance Show Complex where 
    show (Complex r i) 
        | i >= 0 = show r ++ "+" ++ show i ++ "i" 
        | otherwise = show r ++ "-" ++ show (abs i) ++ "i" 

instance Num Complex where
    (+) (Complex a b) (Complex c d) = Complex (a+c) (b+d)
    (*) (Complex a b) (Complex c d) = Complex (a*c - b*d) (a*d + b*c)
    abs (Complex a b) = Complex (sqrt (a*a + b*b)) 0
    signum (Complex a b) = Complex (a / z) (b / z) where z = sqrt (a*a + b*b)
    fromInteger a = Complex (fromIntegral a) 0
    negate (Complex a b) = Complex (-a) (-b)

-- TASK 5
-- Making your own type classes

type Position = (Double, Double)

class Pos a where
    pos :: a -> Position

data Campus = Kalvskinnet
            | Gløshaugen
            | Tyholt
            | Moholt
            | Dragvoll
            deriving (Show, Eq)

instance Pos Campus where
    pos Kalvskinnet = (63.429, 10.388)
    pos Gløshaugen  = (63.416, 10.403)
    pos Tyholt      = (63.423, 10.435)
    pos Moholt      = (63.413, 10.434)
    pos Dragvoll    = (63.409, 10.471)

--class (Pos a) => Move a where
