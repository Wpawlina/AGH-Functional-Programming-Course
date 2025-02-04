--concat

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

doubleConcat:: [[[a]]] -> [a]
doubleConcat x = concat' . concat' $ x


result =concat . map (map length) $ [[[1,2],[3..5]],[[5..6],[7..9]]]
