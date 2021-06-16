module BC where
import Data.List
import Data.Char

--indexInto returns the index of the first argument in a list 
--(don't worry about error checking -- can assume in list)
indexInto :: Eq a => a -> [a] -> Int
indexInto x (y:ys) = maybe 0 (+1) $ x `elemIndex` ys

--converts a character into its corresponding integer value
-- e.g. '0' to 0, 'A' to 10, 'Z' to 35 
-- like hex, except with more digits
-- (consider using elem -- look it up)
alphabet = ['A', 'B'..'Z']
alphabetNums = [10, 11..35]
charNums = ['0', '1'..'9']
nums = [0, 1..9]

alphabetTuple = zip alphabet alphabetNums
numsTuple = zip charNums nums

search :: (Eq a) => a -> [(a,b)] -> b
search _ [] = error "not found"
search x ((a,b):xs) = if x == a then b else search x xs

dig2Int :: Char -> Int
dig2Int dChar = 
    if (dChar `elem` charNums)
        then search dChar numsTuple
        else search dChar alphabetTuple

--converts an integer in range 0..35 into its 
-- corresponding digit (0,1..Z)
-- again, don't care about ints out of bounds

numbers = [0, 1..35]
numsChar = ['0', '1'..'9']
lettersChar = ['A', 'B'..'Z']
charList = numsChar ++ lettersChar
numbersTuple = zip numbers charList

num2char :: Int -> Char
num2char n = search n numbersTuple

--converts an integer value to a string representing
-- the number in base b
-- suggest looking up repeated division strategy
-- for how to convert base 10 to binary and 
-- then generalize
--
findBase a b | a == 0 = ""
             | otherwise = num2char(mod a b) : findBase (div a b) b

int2Base :: Int -> Int -> String
int2Base n b = reverse (findBase n b)

--convert a number string in base b1 into an Int
-- can assume input is valid
valNumString :: String -> Int -> Int
valNumString (xs) b1 = sum (map (\ (x,y) -> (dig2Int x)* (b1^y)) result)
    where
        result = zip (reverse xs) [0..]

--convert String of numbers in base b1 into 
-- equivalent value in base b2, as a String
-- again, all input will be valid
convert :: String -> Int -> Int -> String
convert numString b1 b2 = int2Base (valNumString numString b1) b2
