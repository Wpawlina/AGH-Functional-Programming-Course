--currying, partially apllied functions



myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x, y) = x + y

add2C :: Num a => a -> a -> a
add2C x y = x + y


curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = \a b -> f (a, b)


uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f = \(a,b) -> f a b

fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: Num a => a -> a
_ToPower5 =  (^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ = flip (-) 5


flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f = \x y -> f y x

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f = \x y z -> f z y x

