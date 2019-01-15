{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA)
import Data.Foldable (fold)

newtype Compose f g a = Compose { getCompose :: f (g a) }
                      deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- _ f a       -->   f (g (a -> b)) -> f (g a) -> f (g b)
  -- _ f <*> a   -->   f (g (a -> b)) -> f (g a -> g b)
  (Compose f) <*> (Compose a) = Compose $ ((pure (<*>)) <*> f) <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose c) = (foldMap . foldMap) f c

-- I cheated and looked this up:
-- https://github.com/ekmett/transformers/blob/master/Data/Functor/Compose.hs
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose a) = Compose <$> traverse (traverse f) a


-- ----------------------------------------------------------------------

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id


data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap fl fr (Deux a b) = Deux (fl a) (fr b)

data Const a b = Const a

instance Bifunctor Const where
  first f (Const a) = Const (f a)
  second f (Const a) = Const a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap fl fr (Drei a b c) = Drei a (fl b) (fr c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei c) where
  bimap fl fr (SuperDrei a b) = SuperDrei a (fl b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  first f (SemiDrei a) = SemiDrei a
  second f (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap fl fr (Quadzzz a b c d) = Quadzzz a b (fl c) (fr d)

instance Bifunctor Either where
  first f (Left a) = Left (f a)
  second f (Right a) = Right (f a)


-- ----------------------------------------------------------------------

newtype Identity a =  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
