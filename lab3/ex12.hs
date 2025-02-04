--collection pipeline

import Data.Char
import Data.List

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : (map toLower xs)

formatStr s = foldr1 (\w s -> w ++ " " ++ s) .
          map capitalize .
          filter (\x -> length x > 1) $
          words s

result = replicate 2 . product . map (*3) $ zipWith max [4,2] [1,5]


result2= takeWhile (<1000) . filter odd . map (^2) $ [1..]