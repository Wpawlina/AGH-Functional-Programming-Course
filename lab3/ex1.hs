--funkcje anonimowe

f1 = \x -> x - 2

f2 = \x y -> sqrt (x^2 + y^2)

f3 = \x y z -> sqrt (x^2 + y^2 + z^2)

f4 = \x -> 2 * x

f5= \x -> (^) 2 x

f6 = \x -> x^2

f7 = \x -> if x >= 0 then x else -x

f8 = \x -> let y = sqrt x in 2 * y^3 * (y + 1)

f9 1=3
f9 _=0