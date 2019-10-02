{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
module Continuity.Handler where

import Control.Comonad.Cofree
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time.Clock
import Data.Fixed
import Control.Lens hiding ((:<))
import Control.Monad.State
import Data.Monoid
import Data.Foldable
import Control.Comonad
import GHC.Generics (Generic)

type Animation e s = Cofree (Handler s e) s

buildAnim :: Handler s e () -> s -> Animation e s
buildAnim h s = s :<
  do h
     buildAnim h <$> get

handleEvent :: evt -> Animation evt s -> Animation evt s
handleEvent e (st :< Handler h) = flip evalState st . flip runReaderT e $ h

newtype Handler s e a =
    Handler { handle :: ReaderT e (State s) a
            } deriving newtype (Functor, Applicative, Monad, MonadState s, MonadReader e)
              deriving stock Generic

instance Profunctor (Handler s) where
  dimap l r (Handler m) = Handler . fmap r . ReaderT . lmap l . runReaderT $ m

deriving anyclass instance Wrapped (Handler e s a)

instance Semigroup a => Semigroup (Handler e s a) where
  (Handler f) <> (Handler g) = Handler . getAp $ Ap f <> Ap g

instance Monoid a => Monoid (Handler e s a) where
  mempty = return mempty

handlerFunc :: Iso' (Handler st evt a)  (evt -> State st a)
handlerFunc = _Wrapping' Handler . _Wrapping' ReaderT
focusing :: Prism' e e' -> Lens' (Handler st e a)  (Handler st e' a)
focusing p = handlerFunc . outside p . from handlerFunc

liftHandlerOf :: Monoid r
               => Prism' event subevent
               -> Handler s subevent r
               -> Handler s event r
liftHandlerOf p h = mempty & focusing p <>~ h

continuous :: Monad m => m event -> (state -> m ()) -> Animation event state -> m a
continuous events renderer animation = do
    renderer $ extract animation
    evt <- events
    continuous events renderer (handleEvent evt animation)
