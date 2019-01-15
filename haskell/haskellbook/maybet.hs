{-# LANGUAGE InstanceSigs #-}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> pure Nothing
      Just y -> runMaybeT (f y)

-- ----------------------------------------------------------------------

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap.fmap) f mea

instance (Applicative m) => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT fab) <*> (EitherT ema) = EitherT $ (<*>) <$> fab <*> ema

instance (Monad m) => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ema) >>= f = EitherT $ do
    v <- ema
    case v of
      Left x -> pure (Left x)
      Right x -> runEitherT (f x)

swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ (swapEither <$> ema)

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT amb) = do
  v <- amb
  case v of
    Left x -> fa x
    Right x -> fb x

-- ----------------------------------------------------------------------

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))
  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

-- ----------------------------------------------------------------------

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ do
    m <- g
    \s -> conv f s <$> m

      where
        conv :: (a -> b) -> s -> (a, s) -> (b, s)
        conv f s' (a,_) = (f a, s')

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  -- I looked this up at https://github.com/data61/fp-course/issues/134
  StateT g <*> StateT h = StateT $ \s -> do
    (f, s') <- g s
    (x, s'') <- h s'
    pure (f x, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a,s') <- sma s
    runStateT (f a) s'
