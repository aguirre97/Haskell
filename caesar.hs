-- Alexandra Aguirre
-- Assignment 4
-- Haskell Caesar Cipher
-- COP 4020 Spring 2018
-- 2/12/18

import Data.Char
import Data.List

-- converts a lower-case letter in the range 'a' to 'z' 
-- into the corresponding natural number in the range 0 to 25
let2nat :: Char -> Int
let2nat x = ord x - ord 'a'

-- performs the inverse of let2nat
nat2let :: Int -> Char
nat2let x = chr (ord 'a' + x)

-- applies a shift factor in the range 0 to 25 to a lowercase letter
-- in the range 'a' to 'z', using let2nat and nat2let
-- characters outside the range should return as unshifted
-- make sure function wraps around the end of the alphabet
shift :: Int -> Char -> Char
shift x y | isLower y = nat2let(((let2nat y) + x) `mod` 26)
          | otherwise = y

-- encodes a string using a given shift factor
encode :: Int -> String -> String
encode x ys = [shift x y | y <- ys]

-- helper funtion for decode
-- applies a negative shift factor
decodeShift :: Int -> Char -> Char
decodeShift x y | isLower y = nat2let(((let2nat y) - x) `mod` 26)
          | otherwise = y

-- performs the inverse of encode
decode :: Int -> String -> String
decode x ys = [decodeShift x y | y <- ys]

--  frequency table of how often letters are used 
-- needed for later often
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- calculates the number of lower-case letters in a string
lowers :: String -> Int 
lowers ys = length [y | y <- ys, isLower y]

-- calculates the number of a given character in a string
count :: Char -> String -> Int
count x ys = length [y | y <- ys, x == y]

-- calculates the percentage of one integer with respect to another
-- returning the result as a floating point number
percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

-- returns a list of percentage frequencies of each of the lower-case
-- letters 'a' to 'z' in a string of characters
-- must use lowers, count, and percent
freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- ['a'..'z']]

-- roates a list n places to the left, wrapping around at the start of the list
-- and assuming n is in the range zero to the length of the list
rotate :: Int -> [a] -> [a]
rotate x y = (drop x y) ++ (take x y)

-- calculates the chi square statistic for a list of observed frequencies
--chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- returns the first position (counting from zero) at which a value
-- occurs in a list, assuming that it occurs at least once
position :: Eq a => a -> [a] -> Int
position x xs = head [b | (a,b) <- zip xs [0..(length xs -1)], a == x]

-- attempts to decode a string by first calculating the letter frequencies
-- in the string, then calculating the chi square value of each rotation
-- of this list with respect to the table expected frequencies, and finally
-- using the position of the minimum value in this list to shift the factor to decode 
-- the original string

-- used as a helper function for crack
freqArray s = [chisqr (freqs (decode a s)) table | a <- [0..25]] 

crack :: String -> String
crack s = decode (position (minimum (freqArray s)) (freqArray s)) s
