newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . const . pure
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList l = unDL l []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL ((<> [x]) . unDL xs)
{-# INLINE snoc #-}

-- ----------------------------------------------------------------------

data Queue a = Queue { enqueue :: [a]
                     , dequeue :: [a]
                     } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push a (Queue e d) = Queue (a:e) d

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue a (x:xs)) = Just (x, Queue a xs)
pop (Queue [] []) = Nothing
pop (Queue a []) = let (x:xs) = reverse a in Just (x, Queue [] xs)
