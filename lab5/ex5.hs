
import Data.Sequence (Seq(Empty))

newtype Box a = MkBox a deriving Show

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

newtype Pair a = MkPair {getPair :: (a,a)} deriving Show



instance Functor MyList where
  fmap _ EmptyList    = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)



data BinTree a = EmptyBT | NodeBT a (BinTree a) ( BinTree a) deriving Show


instance Functor BinTree where
    fmap _ EmptyBT = EmptyBT
    fmap f (NodeBT x lt rt) = NodeBT (f x) (fmap f lt) (fmap f rt)

newtype Pair a  = Pair { getPair :: (a,a) }

instance Functor  Pair a  where
    fmap f (Pair {getPair=(x,y)}) = Pair (f x, f y)

data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show

instance Functor Tree2 where
    fmap _ EmptyT2 = EmptyT2
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)