{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Control.Lens.FileSystem.Internal.Combinators where

import Control.Lens
import Control.Lens.Action
import Control.Lens.Action.Internal
import Control.Applicative

-- | If a given fold fails (e.g. with an exception), recover and simply return 0 elements
-- rather than crashing.
recovering :: (Monad m, Alternative m, Monoid r, Effective m r f) => Over' p f s a -> Over' p f s a
recovering fld f s = effective (ineffective (fld f s) <|> pure mempty)

-- | Try the given fold, if it throws an exception then return the input as the output instead
tryOrContinue :: (Monad m, Alternative m) => Acting m r a a -> Acting m r a a
tryOrContinue = flip tryCatch pure

-- | Try the given fold, if it throws an exception then use the given handler to compute a
-- replacement value and continue with that.
tryCatch :: (Monad m, Alternative m) => Acting m r s b -> (s -> m b) -> Acting m r s b
tryCatch fld handler f a = effective (ineffective (fld f a) <|> (handler a >>= ineffective . f))

-- | Filter a fold using a monadic action
filteredM :: (Monad m, Monoid r) => (a -> m Bool) -> Acting m r a a
filteredM predicate f a = effective go
  where
    go = do
      predicate a >>= \case
        True -> ineffective (f a)
        False -> pure mempty

-- | Merge two folds
merging :: (Applicative f, Contravariant f) => LensLike' f s a -> LensLike' f s a -> LensLike' f s a
merging fold1 fold2 nextFold s = fold1 nextFold s *> fold2 nextFold s

-- | Include the results of an additional fold alongside the original values
including :: (Applicative f, Contravariant f) => LensLike' f a a -> LensLike' f a a
including = merging id
