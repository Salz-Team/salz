
data Mylist a = Empty | Cons a (Mylist a)

deriving instance (Show a) => Show (Mylist a)

instance Foldable Mylist where
  foldr f s Empty = s
  foldr f s (Cons a ra) = foldr f (f a s) ra
    

instance Functor Mylist where
  fmap f Empty = Empty
  fmap f (Cons a r) = Cons (f a) (fmap f r)

-- instance Applicative Mylist where
--   pure a = Cons a Empty
--   (<*>) Empty _ = Empty
--   (<*>) (Cons f rf) r = Cons (rf <*> r) (fmap f r)
