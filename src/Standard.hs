{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Standard where

import Control.Lens
import Control.Lens.Action
import Control.Lens.Action.Internal
import System.Directory
import System.FilePath.Posix
import Control.Applicative

contents :: Action IO FilePath String
contents = act readFile

file :: FilePath -> Getter FilePath FilePath
file filePath = to (</> filePath)

dir :: FilePath -> Getter FilePath FilePath
dir = file

ls :: MonadicFold IO FilePath [FilePath]
ls = act (\fp -> (fmap (fp </>)) <$> listDirectory fp)

mv :: FilePath -> MonadicFold IO FilePath ()
mv filePath = act (flip renamePath filePath)

absoluted :: MonadicFold IO FilePath FilePath
absoluted = act makeAbsolute

infixr 8 !%=
(!%=) :: (Monad m) => Acting m b s a -> (a -> m b) -> s -> m b
(!%=) action f = perform (action . act f)

infixr 8 !!%=
(!!%=) :: (Monad m) => Acting m [b] s a -> (a -> m b) -> s -> m [b]
action !!%= f = (^!! action . act f)

infixr 8 !%~
(!%~) :: (Monad m) => Acting m b s a -> (a -> b) -> s -> m b
(!%~) action f = perform (action . to f)

infixr 8 !!%~
(!!%~) :: (Monad m) => Acting m [b] s a -> (a -> b) -> s -> m [b]
action !!%~ f = (^!! action . to f)

-- | 'try' will run a given monadic fold, recovering on failure
--
-- try :: (Alternative m, Monoid r, Effective m r f) => ((a -> f a) -> s -> f s) -> ((a -> f a) -> s -> f s)
tryOrBail :: (Monad m, Alternative m, Monoid r) => Acting m r s a -> Acting m r s a
tryOrBail fld f s = effective (ineffective (fld f s) <|> pure mempty)

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

dirs :: (Monoid r) => Acting IO r FilePath FilePath
dirs = filteredM doesDirectoryExist

files :: (Monoid r) => Acting IO r FilePath FilePath
files = filteredM doesFileExist

symLinked :: Monoid r => Acting IO r FilePath FilePath
symLinked = tryCatch (act getSymbolicLinkTarget) pure

crawled :: Monoid r => Acting IO r FilePath FilePath
crawled = unioned (tryOrBail (ls . traversed . crawled))

-- | ADDS a fold to existing values
-- unioned :: Fold a a -> Fold a a
-- unioned :: Applicative f => ((t -> f a) -> t -> f b) -> (t -> f a) -> t -> f b
-- The Contravariant restraint is redundant; but prevents using this as a traversal which
-- would have unexpected behaviour
unioned :: (Applicative f, Contravariant f) => LensLike f a a a a -> LensLike f a a a a
unioned additionalFold currentFold s = currentFold s *> (additionalFold currentFold s)

data Config = Config
    { _workDir :: FilePath
    }

makeLenses ''Config


main :: IO ()
main = do
    -- fileContents <- "." ^!! file "README.md" . contents . lined
    -- print fileContents
    -- "." ^!! file "README.md" . contents . lined !%~ print
    -- "." &! file "README.md" . contents . lined !%~ print
    -- "." &! file "README.md" !%~ flip renamePath "README2.md"
    -- Config "." & workDir . file "README.md" !%~ flip renamePath "README2.md"
    -- Config "." & workDir . file "README2.md" !%~ flip renamePath "README.md"
    -- r <- Config "." & workDir . file "README.md" !%~ pure
    -- r <- Config "." & workDir . ls . traversed !!%= pure
    -- r <- Config "." & workDir . ls . traversed . try ls . traversed !!%~ id
    -- Config "." ^! workDir . ls . traversed . dirs . act print
    -- Config "." ^! workDir . crawled . symLinked . absoluting . act print
    Config "." ^! workDir . ls . traversed . symLinked . act print
    -- dirContents <- "." ^!! ls . traversed . filteredM doesDirectoryExist
    -- print dirContents
    return ()
