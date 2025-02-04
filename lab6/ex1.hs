import Language.Haskell.TH (safe)
import Control.Monad
--kleisli arrows

(<$<) :: (a->b)-> a -> b
(<$<) = ($)


(>$>) :: a -> (a->b) -> b
x >$> f = f x
infixl 0 >$> 

(<.<) :: (b->c) -> (a->b)-> (a->c)
(<.<) = (.)

(>.>) :: (a->b) -> (b->c) -> (a->c)
f >.> g = g . f
infixl 9 >.>


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs


extractMaybe :: Maybe a -> a
extractMaybe Nothing = error "Nothing inside!"
extractMaybe (Just x) = x

insertMaybe :: a -> Maybe a
insertMaybe = Just

-- = extract (^) and apply ($)
(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >^$> _ = Nothing
(Just x) >^$> f =  f x
infixl 1 >^$>

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing


f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing

--kleisli composition
(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >.>> g = \x -> g (extractMaybe (f x))


(>.>>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >.>>> g =(\x -> f x >^$> g)



--Maybe monad


doSafeTail3x :: [a] -> Maybe [a]
doSafeTail3x xs = do
  t1 <- safeTail xs
  t2 <- safeTail t1
  t3 <- safeTail t2
  return t3

safeTail3x :: [a] -> Maybe [a]
safeTail3x xs =
  safeTail xs >>= \t1 ->
    safeTail t1 >>= \t2 ->
      safeTail t2 >>= \t3 ->
        return t3

safeTail3x' :: [a] -> Maybe [a]
safeTail3x' xs = return xs >>= safeTail >>= safeTail >>= safeTail

f5 :: Int -> Int -> Int -> Int
f5 x y z = 1000 `div` x + 100 `div` y + 10 `div` z


safeDiv :: Int -> Int -> Maybe Int
safeDiv x y | y /= 0    = Just $ x `div` y
            | otherwise = Nothing

safeF5 :: Int -> Int -> Int -> Maybe Int
safeF5 x y z =
  case (safeDiv 1000 x) of
    Nothing -> Nothing
    Just (iOverX) ->
      case (safeDiv 100 y) of
        Nothing -> Nothing
        Just (iOverY) ->
          case (safeDiv 10 z) of
            Nothing -> Nothing
            Just (iOverZ) -> Just $ iOverX + iOverY + iOverZ


safeF5' :: Int -> Int -> Int -> Maybe Int
safeF5' x y z = do
  iOverX <- safeDiv 1000 x
  iOverY <- safeDiv 100 y
  iOverZ <- safeDiv 10 z
  return $ iOverX + iOverY + iOverZ


safeF5'' :: Int -> Int -> Int -> Maybe Int
safeF5'' x y z = f <$> iOverX <*> iOverY <*> iOverZ
  where
    f i j k = i + j + k
    iOverX = safeDiv 1000 x
    iOverY = safeDiv 100 y
    iOverZ = safeDiv 10 z

safeF5''' :: Int -> Int -> Int -> Maybe Int
safeF5''' x y z = safeDiv 1000 x >>= \iOverX ->
                  safeDiv 100 y >>= \iOverY ->
                  safeDiv 10 z >>= \iOverZ ->
                  return $ iOverX + iOverY + iOverZ


sum10DivXi :: [Int] -> Int
sum10DivXi = foldr (\xi acc -> 10 `div` xi + acc) 0

safeSum10DivXi :: [Int] -> Maybe Int
safeSum10DivXi [] = Just 0
safeSum10DivXi (xi:xis) = do
  acc <- safeSum10DivXi xis
  res <- safeDiv 10 xi
  return $ res + acc

safeSum10DivXi' :: [Int] -> Maybe Int
safeSum10DivXi' []= return 0
safeSum10DivXi' (xi:xis) = safeDiv 10 xi >>= \res -> safeSum10DivXi' xis >>= \acc -> return (res + acc)


--Either monad

safeTail' :: [a] -> Either String [a]
safeTail' []     = Left "Empty list!"
safeTail' (x:xs) = Right xs

doSafeTail3xE :: [a] -> Either String [a]
doSafeTail3xE xs = do
  t1 <- safeTail' xs
  t2 <- safeTail' t1
  t3 <- safeTail' t2
  return t3


safeTail3xE :: [a] -> Either String [a]
safeTail3xE xs = safeTail' xs  >>= safeTail' >>= safeTail' >>= return 

safeDivE :: Int -> Int -> Either String Int
safeDivE x y | y /= 0    = Right $ x `div` y
            | otherwise = Left "Cannot div by zero!"

safeF5E :: Int -> Int -> Int -> Either String Int
safeF5E x y z =
  case (safeDivE 1000 x) of
    Left e -> Left e
    Right (iOverX) ->
      case (safeDivE 100 y) of
        Left e -> Left e
        Right (iOverY) ->
          case (safeDivE 10 z) of
            Left e -> Left e
            Right (iOverZ) -> Right $ iOverX + iOverY + iOverZ


safeF5E' :: Int -> Int -> Int -> Either String Int
safeF5E' x y z = do
  iOverX <- safeDivE 1000 x
  iOverY <- safeDivE 100 y
  iOverZ <- safeDivE 10 z
  return $ iOverX + iOverY + iOverZ

safeF5E'' :: Int -> Int -> Int -> Either String Int
safeF5E'' x y z =  safeDivE 1000 x >>= \iOverX -> safeDivE 100 y >>= \iOverY -> safeDivE 10 z >>= \iOverZ -> return $ iOverX + iOverY + iOverZ


-- [] monad

xs1 :: [(Int,Int,Int)]
xs1 = [ (x,y,z) | let xs = [1,2], x <- xs, y <- xs, z <- xs ]

doXs1 :: [(Int,Int,Int)]
doXs1 = do
  let xs = [1,2]
  x <- xs
  y <- xs
  z <- xs
  return (x,y,z)


  xs2 :: [(Int,Int,Int)]
xs2 = [ (x,y,z) | let xs = [1..3], x <- xs, y <- xs, z <- xs, x > y && y > z ]

doXs2 :: [(Int,Int,Int)]
doXs2 = do
  let xs = [1..3]
  x <- xs
  y <- xs
  z <- xs

  return (x,y,z)

doXs2' :: [(Int,Int,Int)]
doXs2' = do
  let xs = [1..3]
  x <- xs
  y <- xs
  z <- xs
  if x > y && y > z
    then return (x,y,z)
    else []


put ::  IO ()
put = do
  l<-getLine
  putStr "Checking "
  if length l <= 10 then putStrLn "OK" else putStrLn "Too long"

put2 :: IO ()
put2 = getLine >>= \l-> putStr "Checking" >>   if length l <= 10 then putStrLn "OK" else putStrLn "Too long"


data MyType a b = L a | R a b

instance Functor (MyType a) where
  fmap f (L a) = L a
  fmap f (R a b) = R a (f b)

instance Applicative (MyType a) where
  pure x = R (error "undefined") x
  (<*>) :: MyType a1 (a2 -> b) -> MyType a1 a2 -> MyType a1 b
  (L a) <*> _ = L a
  (R a f) <*> r = fmap f r


instance Monad (MyType a) where
  (>>=) :: MyType a1 a2 -> (a2 -> MyType a1 b) -> MyType a1 b
  ma >>= k = case ma of
    L a -> L a
    R a b -> case k b of
      L a' -> L a'
      R _ b' -> R a b'


