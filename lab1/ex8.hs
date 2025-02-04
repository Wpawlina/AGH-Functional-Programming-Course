--case of
not' :: Bool -> Bool
not' b = case b of 
    True -> False
    False -> True


absInt n=case (n>=0) of
    True -> n
    _ -> -n




or' :: (Bool,Bool) -> Bool
or' (x,y) = case (x,y) of
    (True,True) -> True
    (True,False) -> True
    (False,True) -> True
    (False,False) -> False
