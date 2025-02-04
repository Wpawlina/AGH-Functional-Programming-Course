funcList :: [Double -> Double]
funcList = [\x -> x, \x -> x^2, \x -> x^3, \x -> if x >= 0 then x else -x, \x -> 1]

evalFuncListAt :: a -> [a ->b ] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 2 * t^2, \t -> 3 * t^2)
