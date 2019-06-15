{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Control.Lens.FileSystem.Combinators where

import Control.Lens
import Control.Lens.Action
import Control.Lens.Action.Internal
import Control.Applicative

-- | 'recovering' will run a given monadic fold, recovering with an empty fold on failure
--
-- try :: (Alternative m, Monoid r, Effective m r f) => ((a -> f a) -> s -> f s) -> ((a -> f a) -> s -> f s)
recovering :: (Monad m, Alternative m, Monoid r) => Acting m r s a -> Acting m r s a
recovering fld f s = effective (ineffective (fld f s) <|> pure mempty)

-- Tries the given fold, if it fails then return the input as output
tryOrContinue :: (Monad m, Alternative m) => Acting m r a a -> Acting m r a a
tryOrContinue = flip tryCatch pure

tryCatch :: (Monad m, Alternative m) => Acting m r s b -> (s -> m b) -> Acting m r s b
tryCatch fld handler f a = effective (ineffective (fld f a) <|> (handler a >>= ineffective . f))

filteredM :: (Monad m, Monoid r) => (a -> m Bool) -> Acting m r a a
filteredM predicate f a = effective go
  where
    go = do
      predicate a >>= \case
        True -> ineffective (f a)
        False -> pure mempty


merging :: (Applicative f, Contravariant f) => LensLike' f s a -> LensLike' f s a -> LensLike' f s a
merging fold1 fold2 nextFold s = fold1 nextFold s *> fold2 nextFold s

including :: (Applicative f, Contravariant f) => LensLike' f a a -> LensLike' f a a
including = merging id

infixr 8 %!
(%!) :: (Monad m) => Acting m b s a -> (a -> m b) -> s -> m b
(%!) action f = perform (action . act f)

infixr 8 %!!
(%!!) :: (Monad m) => Acting m [b] s a -> (a -> m b) -> s -> m [b]
action %!! f = (^!! action . act f)

-- infixr 8 !%~
-- (!%~) :: (Monad m) => Acting m b s a -> (a -> b) -> s -> m b
-- (!%~) action f = perform (action . to f)

-- infixr 8 !!%~
-- (!!%~) :: (Monad m) => Acting m [b] s a -> (a -> b) -> s -> m [b]
-- action !!%~ f = (^!! action . to f)
