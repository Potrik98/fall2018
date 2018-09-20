module Lib
    ( Token(..)
    , Op(..)
    , takeWhile
    , dropWhile
    , break
    , splitOn
    , lex
    , tokenize
    , rpn
    , interpret
    , shunt
    ) where

import Prelude hiding (lex, dropWhile, takeWhile, break)
import Data.Char (isDigit)
import Debug.Trace

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]

takeWhile _ [] = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else takeWhile p xs

dropWhile p = takeWhile (\a -> not $ p a)

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([],[])
break p (x:xs)
        | p x = ([], x:xs)
        | otherwise = (x : fst r, snd r)
        where r = break p xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn a (x:xs)
        | a == x = splitOn a xs
        | otherwise = (x : fst r) : (splitOn a (snd r))
        where r = break (==a) xs

data Token = TokOp Op
           | TokInt Int
           | TokErr
           deriving (Eq, Show)

data Op = Plus
        | Minus
        | Div
        | Mult
        deriving (Show, Eq)

lex :: String -> [String]
lex = splitOn ' '

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

doOperator :: Op -> [Int] -> [Int]
doOperator Plus (a:b:xs) = (a + b):xs
doOperator Minus (a:b:xs) = (b - a):xs
doOperator Div (a:b:xs) = (div b a):xs
doOperator Mult (a:b:xs) = (a * b):xs

token :: String -> Token
token "+" = TokOp Plus
token "-" = TokOp Minus
token "*" = TokOp Mult
token "/" = TokOp Div
token s = case t of
        Just v -> TokInt v
        Nothing -> TokErr
        where t = readMaybe s :: Maybe Int

tokenize :: [String] -> [Token]
tokenize = map token

rpn :: [Token] -> [Int] -> ([Token], [Int])
rpn [] (x:xs) = ([TokInt x], xs)
rpn [] [] = ([], [])
rpn (TokErr:_) _ = ([TokErr], [])
rpn (TokInt a:ts) stack = rpn ts (a:stack)
rpn (TokOp op:ts) stack = rpn ts (doOperator op stack)

interpret :: [Token] -> [Token]
interpret ts = fst $ rpn ts []
                
opLess :: Op -> Op -> Bool
opLess Div Mult = False
opLess Mult Div = False
opLess _ Mult = True
opLess _ Div = True
opLess _ _ = False

shunt :: [Token] -> [Token]
shunt t = shuntInternal t [] []

shuntInternal :: [Token] -> [Token] -> [Token] -> [Token]
shuntInternal [] us _ = us
shuntInternal ((TokInt a):xs) us os = shuntInternal xs ((TokInt a):us) os
shuntInternal (TokOp op:xs) us [] = shuntInternal xs us ((TokOp op):[])
shuntInternal (TokOp op:xs) us (TokOp t:os) =
        if opLess op t then
                shuntInternal (trace "less" ((TokOp op):xs)) ((TokOp t):us) os
        else
                shuntInternal (trace ("xs: " ++ show xs) xs) us ((TokOp op):(TokOp t):os)
