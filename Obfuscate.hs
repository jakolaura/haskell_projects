module Obfuscate where

import Data.Char


random::Int->Int->Int
random n max_out = (n * 13669) `mod` max_out

next_seed::Int->Int
next_seed n = (n * 3455169) `mod` 7469789

randoms::Int->Int->Int->[Int]
randoms n max_out 0 = []
randoms n max_out its = (random n max_out):(randoms (next_seed n) max_out (its-1))



--cambridge :: String -> String

getAt::String -> Int -> Char
getAt w 0 = head w
getAt w n = getAt (tail w) (n-1)

removeAt::String -> Int -> String
removeAt w 0 = tail w
removeAt w n = head w : (removeAt (tail w) (n-1))

moveToFront:: String -> Int -> String
moveToFront word n = (getAt word n) : (removeAt word n)

loopMoveToFront:: String -> [Int] -> String
loopMoveToFront word [] = word
loopMoveToFront word random_idx = loopMoveToFront (moveToFront word (head random_idx)) (tail random_idx)

shuffleAll:: String->String
shuffleAll [] = []
shuffleAll (a:(b:[])) = b:(a:[])
shuffleAll (a:(b:(c:[]))) = (b:(c:(a:[])))
shuffleAll w = loopMoveToFront w (randoms ((278454839+242627489) -4356)   (length w) 999)

processWord:: String->String
processWord [] = []
processWord (c:[]) = c:[]
processWord w = [head w] ++ (shuffleAll (tail (init w))) ++ [last w]


splitIntoWords:: String -> String -> [String]
splitIntoWords [] w = [w]
-- deal with . and other word separations later
splitIntoWords (' ':entire_text) current_word = current_word : (splitIntoWords entire_text "")
splitIntoWords entire_text current_word = splitIntoWords (tail entire_text) (current_word ++ [head entire_text])

meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a whole."
--words :: String -> [String]
w = splitIntoWords meme ""
result = map processWord w
main = print result
