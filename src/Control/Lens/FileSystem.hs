{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Lens.FileSystem
    (
    -- * File System Helpers
      ls
    , ls'ed
    , path
    , pathL
    , branching
    , dirs
    , files
    , contents
    , exts
    , crawled
    , crawling
    , absolute
    , withPerms
    , symLinksFollowed

    -- * Combinators
    , filteredM
    , merging
    , including

    -- ** Exception Handling
    , recovering
    , tryOrContinue
    , tryCatch

    -- * Re-exports
    , (</>)

    , readable
    , writable
    , executable
    , module System.FilePath.Lens
    ) where

import Control.Lens
import Control.Lens.Action
import Control.Lens.FileSystem.Combinators
import System.Directory
import System.FilePath.Posix
import System.FilePath.Lens

-- | List the files at a given directory
-- If the focused path isn't a directory this fold will return 0 results
ls :: Monoid r => Acting IO r FilePath [FilePath]
ls = recovering $ act (\fp -> (fmap (fp </>)) <$> listDirectory fp)

-- | Fold over all files in the given directory.
-- If the focused path isn't a directory this fold will return 0 results
-- This is an alias for @@ls . traversed@@
ls'ed :: Monoid r => Acting IO r FilePath FilePath
ls'ed = ls . traversed


-- | Append a path the end of the current path.
-- This uses `</>` for cross platform compatibility so
-- you don't need leading/trailing slashes here
path :: FilePath -> Getter FilePath FilePath
path filePath = to (</> filePath)

-- | Create a filepath from a list of path segments, then append it to the focused path.
-- @pathL ["a", "b"] == path "a" . path "b" == to (</> "a/b")@
pathL :: [FilePath] -> Getter FilePath FilePath
pathL filePaths = to (</> joinPath filePaths)

-- | "Branch" a fold into many sub-paths.
-- E.g. if we want to crawl into BOTH of @src@ and @test@ directories we can do:
--
-- > "." ^! branching ["src", "test"] . crawled
branching :: [FilePath] -> Fold FilePath FilePath
branching filePaths = folding (\fp -> (fp </>) <$> filePaths)

-- | Filter for only paths which point to a valid directory
dirs :: (Monoid r) => Acting IO r FilePath FilePath
dirs = filteredM doesDirectoryExist

-- | Filter for only paths which point to a valid file
files :: (Monoid r) => Acting IO r FilePath FilePath
files = filteredM doesFileExist

-- | Get the contents of a file
-- This fold will return 0 results if the path does not exist, if it isn't a file, or if
-- reading the file causes any exceptions.
--
-- This fold lifts the path of the current file into the index of the fold in case you need it
-- downstream.
contents :: (Monoid r) => Acting IO r FilePath String
contents = selfIndex . recovering (act readFile)

-- | Filter the fold for only files which have ANY of the given file extensions.
-- E.g. to find all Haskell or Markdown files in the current directory:
--
-- > "." ^! crawled . exts ["hs", "md"]
-- exts :: Monoid r => [String] -> Acting IO r FilePath FilePath
exts :: [String] -> Traversal' FilePath FilePath
exts extList = filtered check
  where
    check fp = drop 1 (takeExtension fp) `elem` extList

-- | Crawl over every file AND directory in the given path.
crawled :: Monoid r => Acting IO r FilePath FilePath
crawled = including (dirs . ls . traversed . crawled)

-- continually run the given fold until all branches hit dead ends,
-- yielding over all elements encountered the way.
crawling :: Monoid r => Acting IO r FilePath FilePath -> Acting IO r FilePath FilePath
crawling fld = including (recovering (fld . crawling fld))

-- | Make filepaths absolute in reference to the current working directory
absolute :: MonadicFold IO FilePath FilePath
absolute = act makeAbsolute

-- | Filter for only paths which have ALL of the given file-permissions
-- See 'readable', 'writable', 'executable'
withPerms :: Monoid r => [Permissions -> Bool] -> Acting IO r FilePath FilePath
withPerms permChecks = filteredM checkAll
  where
    checkAll fp = do
        perms <- getPermissions fp
        return $ all ($ perms) permChecks

-- | If the path is a symlink, rewrite the path to its destination and keep folding
-- If it's not a symlink; pass the path onwards as is.
symLinksFollowed :: Monoid r => Acting IO r FilePath FilePath
symLinksFollowed = tryOrContinue (act getSymbolicLinkTarget)
