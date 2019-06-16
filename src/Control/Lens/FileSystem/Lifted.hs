{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Lens.FileSystem.Lifted where

import Control.Lens
import Data.Functor.Compose
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class

contents' :: MonadIO f => (T.Text -> f T.Text) -> FilePath -> f FilePath
contents' f fp = do
    txt <- liftIO $ T.readFile fp
    newText <- f txt
    liftIO $ T.writeFile fp newText
    return fp

type GettingM r s a = forall m. Applicative m => (a -> Compose m (Const r) a) -> s -> Compose m  (Const r) s
viewM :: forall m a s. Applicative m => GettingM a s a -> s -> m a
viewM l s = fmap getConst . getCompose $ l liftConstM s
  where
    liftConstM :: a -> Compose m (Const a) a
    liftConstM a = Compose (pure (Const a))

newtype GetterM m r s a = GetterM (Compose m (Const r) a)
    deriving (Functor, Applicative)


-- instance (MonadIO m, Monoid r) => MonadIO (GetterM m r s) where
--   liftIO m = GetterM $ Compose (liftIO m >>= pure . Const)
