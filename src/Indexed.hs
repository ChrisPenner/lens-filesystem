{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Indexed where

import Control.Lens
import Control.Lens.Action
import Control.Lens.Action.Internal
import Control.Lens.Action.Type
import System.Directory
import System.FilePath.Posix
import Data.Functor

-- data File = File{ filePath :: FilePath, contents :: String }
data FileType = Dir | File | SymLink
data Location = Location{ locPath :: FilePath, locType ::  FileType}

-- An action which requires a specific index type as input
type IndexUsingAction i m s a = forall p r f. (Conjoined p, Effective m r f) => Indexed FilePath (p String (f String)) (p a (f a))
type IndexUsingFileAction s a = IndexUsingAction FilePath IO s a

type Blah i m s a = forall f r s p. (Effective m r f, Indexable i p) => (p a (f a)) -> Indexed i s (f s)


contents :: forall f r s p. (Effective IO r f, Indexable FilePath p) => (p String (f String)) -> Indexed FilePath s (f s)
contents overContents = Indexed go
  where
    go fp s = effective (readFile fp >>= ineffective . indexed overContents fp) $> s

    -- asIndex . contents'

-- contents :: forall f p r. (Functor f, Indexable FilePath p, Effective IO r f) => p String (f String) -> Indexed FilePath Location (f Location)

-- contents :: IndexUsingFileAction s String
-- contents = Indexed go
--   where
--     go fp = act (const $ readFile fp)

file :: (Indexable FilePath p, Contravariant f, Functor f) => FilePath -> p s (f s) -> Indexed FilePath s (f s)
file filePath = withIndex . ito addFile
  where
    addFile (pth, next) = (pth </> filePath, next)

file' :: Indexable FilePath p => FilePath -> (Indexed FilePath a b -> r) -> p a b -> r
file' filePath = reindexed (</> filePath)

-- file' :: (Indexable FilePath p, Contravariant f, Functor f) => FilePath -> p s (f s) -> Indexed FilePath s (f s)
-- file' :: FilePath -> (Indexable FilePath p, Contravariant f) => Over' p f s a
-- file :: (Indexable FilePath p, Contravariant f, Functor f) => FilePath -> p s (f s) -> Indexed FilePath s (f s)
-- file filePath = reindexed (</>filePath)




-- file :: FilePath -> Getter Location Location
-- file :: IndexPreservingGetter Location Location
-- file path = to go
--   where
--     go (Location pth typ) = Location (pth </> path) typ

ls :: forall f r s p. (Effective IO r f, Indexable FilePath p) => (p [FilePath] (f [FilePath])) -> Indexed FilePath s (f s)
ls overContents = Indexed go
  where
    go dirPath s = effective (listDirectory dirPath >>= ineffective . indexed overContents dirPath) $> s

-- This is more restrictive than it needs to be
dir ::
  FilePath
  -> Indexed FilePath s (Effect m r s)
  -> Indexed FilePath s (Effect m r s)
dir = file

relativeTo :: forall p f a. FilePath -> (Indexable FilePath p, Contravariant f) => Over' p f a a
relativeTo pth = ito (\a -> (pth, a))

-- p a (f b) -> p s (f t)
-- filteredM :: (Choice p, Applicative f) => (a -> m Bool) -> Optic' p f a a
--Action m s a = forall f r. Effective m r f => (a -> f a) -> s -> f s
-- filteredM :: (a -> m Bool) -> Action m a a
filteredM :: (Effective m r f) => (r -> m Bool) -> (r -> f a) -> r -> f a
filteredM predicate f a = effective go
  where
    -- go :: IO a
    go = do
        b <- (predicate a)
        if b then ineffective $ f a
             else pure a

-- indicesM

main :: IO ()
main = do
    -- stuff <- () ^!! relativeTo "." . file "README.md" . contents . lined
    fileContents <- () ^!! relativeTo "." . file "README.md" . contents . lined
    print fileContents
    dirContents <- () ^! relativeTo "." . dir "src" . ls
    print dirContents
    -- stuff2 <- () ^! relativeTo "/Users/chris" . file ".bashrc" . contents . lined
    return ()
    -- (Location "README.md" File) ^!! relativeTo "/Users/chris" . file' ".bashrc" -- . contents . lined


-- fs !.. dir "." . filtered ((~~ "pictures.*") . name) . contents . subdir "hawaii" . crawled . files . contents
-- fs & dir "." . glob "*.md" !%~ uppercase
--
-- fs !.. dir "." . filtered ((~~ "pictures.*") . name) . contents . subdir "hawaii" . crawled . files . contents
-- dir "." . glob "*.md" !%= uppercase
-- relative !. file "hello.txt"




