{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
module Lib where

import Control.Lens
import Control.Lens.Action
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


contents' :: Action IO FilePath String
contents' = act action
  where
    action (absPth) = readFile absPth

contents = asIndex . contents'

-- contents :: forall f p r. (Functor f, Indexable FilePath p, Effective IO r f) => p String (f String) -> Indexed FilePath Location (f Location)

-- contents :: IndexUsingFileAction s String
-- contents = Indexed go
--   where
--     go fp = act (const $ readFile fp)

file :: (Indexable FilePath p, Contravariant f, Functor f) => FilePath -> p s (f s) -> Indexed FilePath s (f s)
file filePath = withIndex . ito addFile
  where
    addFile (pth, next) = (pth </> filePath, next)

-- file' :: (Indexable FilePath p, Contravariant f, Functor f) => FilePath -> p s (f s) -> Indexed FilePath s (f s)
-- file' :: FilePath -> (Indexable FilePath p, Contravariant f) => Over' p f s a
-- file :: (Indexable FilePath p, Contravariant f, Functor f) => FilePath -> p s (f s) -> Indexed FilePath s (f s)
-- file filePath = reindexed (</>filePath)




-- file :: FilePath -> Getter Location Location
-- file :: IndexPreservingGetter Location Location
-- file path = to go
--   where
--     go (Location pth typ) = Location (pth </> path) typ

relativeTo :: forall p f a. FilePath -> (Indexable FilePath p, Contravariant f) => Over' p f a a
relativeTo pth = ito (\a -> (pth, a))

main = do
    (Location "README.md" File) ^!! relativeTo "/Users/chris" . file ".bashrc" . contents . lined


-- fs !.. dir "." . filtered ((~~ "pictures.*") . name) . contents . subdir "hawaii" . crawled . files . contents
-- fs & dir "." . glob "*.md" !%~ uppercase
--
-- fs !.. dir "." . filtered ((~~ "pictures.*") . name) . contents . subdir "hawaii" . crawled . files . contents
-- dir "." . glob "*.md" !%= uppercase
-- relative !. file "hello.txt"




