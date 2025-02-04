--polimorfism
roots2 :: (Fractional a,Floating a) => (a, a, a) -> (a, a)
roots2 (a,b,c) = ((-b - d) / e, (-b + d) / e)
    where {d = sqrt(b*b - 4*a*c); e = 2*a}

