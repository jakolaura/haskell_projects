module Char where

import Data.Char

(~~) :: String -> String -> Bool
s1 ~~ s2 = map toLower s1 == map toLower s2

rev :: Char -> Char
rev c 
  | isLower c = toUpper c
  | isUpper c = toLower c
  | otherwise = c

reverseCase :: String -> String
reverseCase = map rev

shift :: Int -> Char -> Char
shift n c = 
	| isUpper c = 
		let base = ord 'A' 
		offset = ord c - base 
		shiftedOffset = (offset + n)`mod` 26
		shiftedAscii = base + shiftedOffset
       in chr shiftedAscii
    |otherwise = c 
	

caesar :: Int -> String -> String
caesar n = map (shift n . toUpper) 


msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
