--fold

strList1 = ["My", "name", "is", "Inigo", "Montoya"]

result= foldr (++) [] strList1

result2 = foldr (\acc x-> acc ++ " " ++ x ) [] strList1


map' ::(a->a)->[a]->[a]
map' f xs = foldr (\x acc -> f x : acc) [] xs


filter':: (a->Bool)->[a]->[a]
filter' f xs = foldr (\x acc -> if f x then acc ++ [x] else acc) [] xs