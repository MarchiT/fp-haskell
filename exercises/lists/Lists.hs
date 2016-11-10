{-# OPTIONS_GHC -Wall #-}

module Lists where

import qualified Data.List ()

import Prelude hiding (
        head, tail, null, length, reverse, repeat, replicate,
        concat, sum, maximum, take, drop, elem, (!!)
    )


head :: [Int] -> Int
head []    = error "empty list"
head (x:_) = x


tail :: [Int] -> [Int]
tail []     = error "empty list"
tail (_:xs) = xs


append :: [Int] -> [Int] -> [Int]
append []     ys = ys
append (x:xs) ys = x : (append xs ys)


elementAt :: Int -> [Int] -> Int
elementAt 0 (x:_) = x
elementAt n _
    | n < 0 = error "negative index"
elementAt _ [] = error "index greater than length"
elementAt n (_:xs) = elementAt (n - 1) xs

null :: [Int] -> Bool
null [] = True
null _  = False

length :: [Int] -> Int
length [] = 0
length (_:xs) = 1 + (length xs)


take :: Int -> [Int] -> [Int]
take _ [] = []
take 1 (x:_) = [x]
take n _ | n < 1 = []
take n (x:xs) = x : take (n-1) xs

take' :: Int -> [Int] -> [Int]
take' 0 _  = []
take' _ [] = [] 
take' n (x:xs) | n < 0     = []
               | otherwise = x : take' (n-1) xs


drop :: Int -> [Int] -> [Int]
drop _ [] = []
drop n xs | n < 0  = xs
          | n == 0 = xs
drop n (_:xs) = drop (n-1) xs


elem :: Int -> [Int] -> Bool
elem _ [] = False
elem n (x:xs) | n == x = True
              | otherwise = elem n xs


reverseHelper :: [Int] -> [Int] -> [Int]
reverseHelper acc []     = acc
reverseHelper acc (x:xs) = reverseHelper (x:acc) xs

reverse :: [Int] -> [Int]
reverse xs = reverseHelper [] xs

reverseStringHelper::String->String->String
reverseStringHelper acc [] = acc
reverseStringHelper acc (x:xs) = (reverseStringHelper (x:acc) xs)


reverseString :: String -> String
reverseString str = "This is the reversed string: " ++ (reverseStringHelper [] str)

concat :: [[Int]] -> [Int]
concat [] = []
concat (x:xs) = append x (concat xs)


replicate :: Int -> Int -> [Int]
replicate n x | n <= 0 = []
              | otherwise = x : replicate (n-1) x


interleave :: [Int] -> [Int] -> [Int]
interleave [] _ = []
interleave (x:_) [] = [x]
interleave (x:xs) ys = x : interleave ys xs

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs


maximum :: [Int] -> Int
maximum [] = error "empty list"
maximum (x:xs)
    | null xs = x
    | x > head xs = maximum (x : tail xs)
    | otherwise = maximum xs


nub :: [Int] -> [Int]
nub [] = []
nub (x:xs) | elem x xs = nub xs
           | otherwise = x : nub xs 


delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete n (x:xs) 
    | x == n = xs
    | otherwise = x : delete n xs


difference :: [Int] -> [Int] -> [Int]
difference [] _ = []
difference (x:xs) ys
    | elem x ys = difference xs (delete x ys)
    | otherwise = x : difference xs ys


union :: [Int] -> [Int] -> [Int]
union xs ys = append xs (difference (nub ys) xs)


intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect (x:xs) ys
    | elem x ys = x : intersect xs ys
    | otherwise = intersect xs ys

