import Data.Char(toLower)

isChar :: Char -> Bool
isChar c | c >= 'a' && c <= 'z' = True
         | c >= 'A' && c <= 'Z' = True
         | otherwise = False


isVowel :: Char -> Bool
isVowel v | toLower v == 'a' || toLower v == 'e' || toLower v == 'o' || toLower v == 'i' || toLower v == 'u' = True
          | otherwise = False

isConsonant :: Char -> Bool
isConsonant c | isChar c && not (isVowel c) = True
              | otherwise = False

encode :: String -> String
encode [] = []
encode (x:xs)
    | isConsonant x = x : 'o' : toLower x : encode xs
    | otherwise   = x : encode xs

decode :: String -> String
decode []       = []
decode (x:y:z:xs)
  |(isConsonant x && isConsonant z) && (toLower x == toLower z) && y == 'o' = x : decode xs
  |otherwise = x : decode (y:z:xs)
decode (x:rest) = x : decode rest
