--typy algebraiczne 

data CartInt2DVec = MkCartInt2DVec Int Int -- konstruktor

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCartInt2DVec' {x::a, y::a}

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCartInt2DVec' {x = xVal, y = _}) = xVal

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCartInt2DVec' {x = _, y = yVal}) = yVal

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL = error "head': the empty list has no head!"
head' (Cons x xs) = x


data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String
leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red = "Irène Jacob"




data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b

data Tree a = EmptyTree | Node a (Tree a ) (Tree a) deriving Show


root :: Tree a -> a
root EmptyTree = error "root: the empty tree has no root!"
root (Node r _ _) = r


inOrderTravelsal :: Tree a -> [a]
inOrderTravelsal EmptyTree = []
inOrderTravelsal (Node a left right)= inOrderTravelsal left ++ [a] ++ inOrderTravelsal right


postOrderTravelsal :: Tree a -> [a]
postOrderTravelsal EmptyTree = []
postOrderTravelsal (Node a left right)= postOrderTravelsal left ++ postOrderTravelsal right ++ [a]

preOrderTravelsal :: Tree a -> [a]  
preOrderTravelsal EmptyTree = []
preOrderTravelsal (Node a left right)= [a] ++ preOrderTravelsal left ++ preOrderTravelsal right


data TrafficLight = Red' | Yellow | Green

actionFor :: TrafficLight -> String
actionFor Red' = "Stop"
actionFor Yellow = "Prepare"
actionFor Green = "Go"


data BinIntTree = EmptyIntBinTree |
     IntBinTree Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBinTree = 0
sumBinIntTree (IntBinTree n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt


depthOfBT :: BinIntTree -> Int
depthOfBT EmptyIntBinTree = 0
depthOfBT (IntBinTree _ lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

mapBT:: (Int -> Int) -> BinIntTree -> BinIntTree
mapBT _ EmptyIntBinTree = EmptyIntBinTree
mapBT f (IntBinTree n lt rt) = IntBinTree (f n) (mapBT f lt) (mapBT f rt)


insertBT :: Int -> BinIntTree -> BinIntTree
insertBT n EmptyIntBinTree = IntBinTree n EmptyIntBinTree EmptyIntBinTree
insertBT n (IntBinTree n' lt rt) | n < n' = IntBinTree n' (insertBT n lt) rt
                                 | n > n' = IntBinTree n' lt (insertBT n rt)
                                 | otherwise = IntBinTree n' lt rt


list2BST :: [Int] -> BinIntTree
list2BST [] = EmptyIntBinTree
list2BST (x:xs) = insertBT x (list2BST xs)


foldBt :: (a->b->b->b) -> b -> Tree a -> b
foldBt _ acc EmptyTree = acc
foldBt f acc (Node n lt rt) = f n (foldBt f acc lt) (foldBt f acc rt)

data GTree a = Leaf a |
               GNode a [GTree a]
               deriving Show

sumGTree :: Num a => GTree a -> a
sumGTree (Leaf n) = n
sumGTree (GNode n ts) = n + sum (map sumGTree ts)



data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"


instance Eq a => Eq (Tree a) where
  (==) EmptyTree EmptyTree = True
  (==) (Node n1 lt1 rt1) (Node n2 lt2 rt2) = n1 == n2 && lt1 == lt2 && rt1 == rt2
  (==) _ _ = False








