
import Data.Char (toUpper)

isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = if x == last xs 
    then isPalindrome (init xs)
     else False



getElemAtIdx :: [a] -> Int -> a
getElemAtIdx (x:xs) 0 = x
getElemAtIdx (x:xs) n = getElemAtIdx xs (n-1)

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
