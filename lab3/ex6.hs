--apply operator

sortDesc :: Ord a => [a] -> [a]
sortDesc x  = reverse $ sort $ x