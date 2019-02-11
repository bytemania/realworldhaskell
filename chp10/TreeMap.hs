module TreeMap where

data Tree a = Node (Tree a) (Tree a) | Leaf a  deriving Show

treeLengths :: Tree [a] -> Tree Int
treeLengths (Leaf l) = Leaf (length l)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

tree :: Tree String
tree = Node (Leaf "foo") (Node (Leaf "x") (Leaf "quux"))

instance Functor Tree where
    fmap = treeMap



