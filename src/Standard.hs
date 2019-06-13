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
import Control.Lens.Internal.Bazaar
import Control.Lens.Action.Type
import System.Directory
import System.FilePath.Posix
import Data.Functor
import Control.Monad
import Control.Applicative

-- data File = File{ filePath :: FilePath, contents :: String }
data FileType = Dir | File | SymLink
data Location = Location{ locPath :: FilePath, locType ::  FileType}

-- An action which requires a specific index type as input
type IndexUsingAction i m s a = forall p r f. (Conjoined p, Effective m r f) => Indexed FilePath (p String (f String)) (p a (f a))
type IndexUsingFileAction s a = IndexUsingAction FilePath IO s a

contents :: Action IO FilePath String
contents = act readFile

file :: FilePath -> Getter FilePath FilePath
file filePath = to (</> filePath)

dir :: FilePath -> Getter FilePath FilePath
dir = file

-- ls :: MonadicFold IO FilePath [FilePath]
ls :: MonadicFold IO FilePath [FilePath]
ls = act listDirectory

mv :: FilePath -> MonadicFold IO FilePath ()
mv filePath = act (flip renamePath filePath)


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

-- filteredM :: (Choice p, Applicative f, Effective m r f) => (a -> m Bool) -> Optic' p f a a
--  -- Optic p f s t a b = p a (f a) -> p s (f s)
-- filteredM p pafb = pafb
--   where
--     other = undefined
    -- choose x = effective $ do
    --     p x <&> \case
    --       True -> Right x
    --       True -> Left x


-- filteredM :: forall m r f a. (Effective m a f) => (a -> m Bool) -> (a -> f a) -> a -> f a
-- filteredM predicate f a = effective go
--   where
--     go = do
--         b <- (predicate a)
--         if b then ineffective $ f a
--              else pure a

-- filteredM :: forall m r f a. (Effective m a f) => (a -> m Bool) -> (a -> f a) -> a -> f a
-- filteredM predicate f a = effective go
--   where
--     go = do
--         b <- (predicate a)
--         if b then ineffective $ f a
--              else pure a

-- | 'try' will run a given monadic fold, recovering on failure
-- TODO: generalize this to not need monoid
--
-- try :: (Alternative m, Monoid r, Effective m r f) => ((a -> f a) -> s -> f s) -> ((a -> f a) -> s -> f s)
try :: (Monad m, Alternative m, Monoid r) => Acting m r s a -> Acting m r s a
try fld f s = effective (ineffective (fld f s) <|> pure mempty)

-- |
iterating :: (Monad m, Alternative m) => Acting m r a a -> Acting m r a a
iterating fld f a = effective (ineffective (fld f a) <|> ineffective (f a))

-- filteredM :: (Monad m, Monoid r) => (a -> m Bool) -> Acting m r a a
filteredM predicate f a = effective go
  where
    go = do
      predicate a >>= \case
        True -> ineffective (f a)
        False -> pure mempty

crawl :: Monoid r => Acting IO r FilePath FilePath
crawl = iterating (ls . traversed . crawl)

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
    r <- Config "." ^! workDir . ls . traversed . filteredM doesDirectoryExist . act print
    print r
    -- dirContents <- "." ^!! ls . traversed . filteredM doesDirectoryExist
    -- print dirContents
    return ()
