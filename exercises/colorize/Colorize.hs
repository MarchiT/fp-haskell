{-# OPTIONS_GHC -Wall #-}

module Colorize where


backgroundStyle, textStyle :: Int
textStyle       = 30
backgroundStyle = 40


black, red, green, yellow, blue, magenta, cyan, white :: Int
black   = 0
red     = 1
green   = 2
yellow  = 3
blue    = 4
magenta = 5
cyan    = 6
white   = 7


clear :: Int
clear   = 0


-- Exercise 1

getLastDigit :: Int -> Int
getLastDigit num = mod num 10


dropLastDigit :: Int -> Int
dropLastDigit num = quot num 10


-- Exercise 2

getReverseDigits :: Int -> [Int]
getReverseDigits x | x < 0  = getReverseDigits (-x)
                   | x < 10 = [x]
                   | otherwise = getLastDigit x : getReverseDigits (dropLastDigit x)

-- Exercise 3

toChar :: Int -> Char
toChar 0 = '0'
toChar 1 = '1'
toChar 2 = '2'
toChar 3 = '3'
toChar 4 = '4'
toChar 5 = '5'
toChar 6 = '6'
toChar 7 = '7'
toChar 8 = '8'
toChar 9 = '9'
toChar _ = error "Not a digit"

-- Exercise 4

itoaLoop :: String -> [Int] -> String
itoaLoop acc [] = acc
itoaLoop acc (x:xs) = itoaLoop (toChar x : acc) xs


itoa :: Int -> String
itoa x = itoaLoop "" (getReverseDigits x)


-- Exercise 5

mkStyle :: Int -> String
mkStyle style = "\x1B[" ++ itoa style ++ "m"


mkTextStyle :: Int -> String
mkTextStyle color = mkStyle (color + textStyle)


getStyle :: String -> String
getStyle "blk" = mkTextStyle black
getStyle "red" = mkTextStyle red
getStyle "grn" = mkTextStyle green
getStyle "ylw" = mkTextStyle yellow
getStyle "blu" = mkTextStyle blue
getStyle "mgt" = mkTextStyle magenta
getStyle "cyn" = mkTextStyle cyan
getStyle "wht" = mkTextStyle white

getStyle "bgr-blk" = mkBackgroundStyle black
getStyle "bgr-red" = mkBackgroundStyle red
getStyle "bgr-grn" = mkBackgroundStyle green
getStyle "bgr-ylw" = mkBackgroundStyle yellow
getStyle "bgr-blu" = mkBackgroundStyle blue
getStyle "bgr-mgt" = mkBackgroundStyle magenta
getStyle "bgr-cyn" = mkBackgroundStyle cyan
getStyle "bgr-wht" = mkBackgroundStyle white

getStyle "clr" = mkStyle clear
getStyle xs = "<" ++ xs ++ ">"


-- Exercise 6

removeStyle :: String -> String
removeStyle text = drop 1 (dropWhile (/= 'm') text)


bleach :: String -> String
bleach ('\x1B':rest) = bleach (removeStyle rest)
bleach (x:rest)      = x : bleach rest
bleach []            = []


colorize :: String -> String
colorize ('<':x:y:z:'>':rest) = getStyle [x,y,z] ++ colorize rest
colorize (x:xs)              = x : colorize xs
colorize []                  = []

-- Extra

mkBackgroundStyle :: Int -> String
mkBackgroundStyle color = mkStyle (color + backgroundStyle)

--takeWhile
getMarkup :: String -> String
getMarkup text = (takeWhile (/= '>') text)

dropMarkup :: String -> String
dropMarkup text = drop 1 (dropWhile (/= '>') text)


-- What bug does `colorize2` have? It's good that we know it, but we won't fix it now

colorize2 :: String -> String
colorize2 ('<':rest) = getStyle (getMarkup rest) ++ colorize2 (dropMarkup rest)
colorize2 (x:xs)     = x : colorize2 xs
colorize2 [] = []
