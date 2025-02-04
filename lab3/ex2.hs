sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum'=sumWith (\x -> x)
sumSqr = sumWith (\x -> x^2)
sumCube = sumWith (\x -> x^3)
sumAbs = sumWith (\x -> if x >= 0 then x else -x)
listLength = sumWith (\x -> 1)

