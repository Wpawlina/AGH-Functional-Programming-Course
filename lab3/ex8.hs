--map


map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElements = map' (\x -> x * 2)
squareElements = map' (\x -> x ^ 2)
lowerCase  s = map' toLower  s  

