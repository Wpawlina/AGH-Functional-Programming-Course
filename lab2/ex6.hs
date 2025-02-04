--rekurencja

{-# LANGUAGE BangPatterns #-}

fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)




fib2 :: Int -> Int
fib2 n = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
 in fibs !! n

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

prod' :: Num a=> [a]-> a
prod' []=1
prod' (x:xs)=x* prod' xs



or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = 2 * x : doubleAll xs

doubleAll2 :: Num t => [t] -> [t]
doubleAll2 x=loop x
  where
    loop  [] = []
    loop (x:xs)=2*x:loop xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = if x `mod` 2 == 0 then x : selectEven xs else selectEven xs


sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x * acc) xs


length'2 :: [a] -> Int
length'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (_:xs) = loop (1 + acc) xs



isOdd :: (Ord a, Num a) => a -> Bool
isOdd n | n <= 0    = False
        | otherwise = isEven (n-1)

isEven :: (Ord a, Num a) => a -> Bool
isEven n | n < 0     = False
         | n == 0    = True
         | otherwise = isOdd (n-1)



ackerFun m n
 | m == 0    = n + 1
 | n == 0    = ackerFun (m - 1) 1
 | otherwise = ackerFun (m - 1) (ackerFun m (n - 1))


qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = [ y | y <- xs, y <= x ]
   rightPart xs = [ y | y <- xs, y > x  ]


qSort2 :: Ord a => [a] -> [a]
qSort2 []     = []
qSort2 (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<= x) xs
   rightPart xs = filter (> x) xs

mSort:: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort left) (mSort right)
 where
   (left, right) = splitAt (length xs `div` 2) xs
   merge [] ys = ys
   merge xs [] = xs
   merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                       | otherwise = y : merge (x:xs) ys


iSort :: Ord a => [a] -> [a]  
iSort [] = []
iSort (x:xs) = insert x (iSort xs)
 where
   insert x [] = [x]
   insert x (y:ys) | x <= y    = x : y : ys
                   | otherwise = y : insert x ys

sSort :: Ord a => [a] -> [a]
sSort [] = []
sSort xs= minimum xs : sSort (remove (minimum xs) xs)
 where
   remove _ [] = []
   remove x (y:ys) | x == y    = ys
                   | otherwise = y : remove x ys

bSort :: Ord a => [a] -> [a]
bSort [] = []
bSort xs = loop (length xs) xs
 where
   loop 0 xs = xs
   loop n xs = loop (n - 1) (bubble xs)
   bubble [x] = [x]
   bubble (x:y:xs) | x > y     = y : bubble (x:xs)
                   | otherwise = x : bubble (y:xs)



concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x  ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' xss = [x | xs <-xss, x<-xs]


isSorted ::Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


zip' :: [a] -> [b] -> [(a,b)] 
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):xys) = (x:xs, y:ys)
 where
   (xs,ys) = unzip' xys

subList :: Eq a => [a] -> [a] -> Bool 
subList [] _ = True
subList _ [] = False
subList (x:xs) (y:ys) | x == y    = subList xs (take (min (length xs) (length ys)) ys) || subList (x:xs) ys
                      | otherwise = subList (x:xs) ys

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

div2 ::  [Int]->Bool
div2 (x:y:_) | y `mod` x == 0 = True
div2 _ = False

div3 ::  [Int]->Bool
div3 (x:y:z:_) |   z `mod` x == 0 = True
div3 _ = False