--filter

import Data.Char (isUpper, isLower)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f []=[]
filter' f (x:xs) | f x = x : filter' f xs
                 | otherwise = filter' f xs

onlyEven =filter' even 
onlyOdd = filter' odd
onlyUpper = filter' isUpper
onlyLower = filter' isLower


f1= filter (\s -> length s == 2) ["aa", "bbb", "cc", "d"]
f2=filter (\(x,y) -> x>y)[(1,2), (2,2), (2,1), (1,1)]
f3=filter (\xs -> sum xs > 10) [[1,2,3], [4,5,6], [7,8,9]]
f4= length . filter (\f -> f 2 > 10) $ [(+5), (*5), (^5),\x -> 3 * x +7]

