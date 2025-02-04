--zip zipWith

isSortedAsc:: Ord a => [a] -> Bool
isSortedAsc xs = zip xs (tail xs) == filter (\(x,y) -> x <= y) (zip xs (tail xs))

