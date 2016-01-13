module FunctorSolutions where

data Pair a = Pair a a

instance Functor (Either a) where
  fmap _ (Left l) = Left l
  fmap g (Right r) = Right (g r)

instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)

instance Functor ((,) e) where
  fmap g (a, b) = (a, g b)

instance Functor ITree where
  fmap g (Leaf h) = Leaf (g . h)
  fmap g (Node ts) = Node (map (fmap g) ts)


