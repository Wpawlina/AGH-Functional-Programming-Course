--wyrażenia warunkowe
sgn n = if n <0
    then -1
    else if n==0
    then 0
    else 1


absInt:: Int -> Int
absInt x = if x > 0 then x else (-x)


min2Int :: (Int,Int) -> Int
min2Int (x,y) = if x > y then  x else y

min3Int :: (Int,Int,Int) -> Int
min3Int (x,y,z) = if x>y && x>z then x else min2Int (y,z)

toUpper :: Char -> Char
toUpper c = if c>='a' && c<='z' then toEnum(fromEnum c -32) else c

isDigit :: Char -> Bool
isDigit x =  x>='0' && x<='9'

charToNum :: Char -> Int
charToNum '1' = 1
charToNum '2' = 2
charToNum c = fromEnum c - fromEnum '0'