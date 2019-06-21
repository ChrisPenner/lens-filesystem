{-|
Module      : Control.Lens.FileSystem
Description : Lensy File system combinators
Copyright   : (c) Chris Penner, 2019
License     : BSD3

Note that this package is experimental, test things carefully before performing destructive
operations. I'm not responsible if things go wrong.

This package is meant to be used alongside combinators from 'lens-action'; for example
'^!', '^!!' and 'act'.
-}


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
import Control.Lens.FileSystem.Internal.Combinators
import System.Directory
import System.FilePath.Posix
import System.FilePath.Lens

-- | List the files at a given directory
-- If the focused path isn't a directory this fold will return 0 results
--
-- >>> "./test/data" ^! ls
-- ["./test/data/flat","./test/data/symlinked","./test/data/.dotfile","./test/data/permissions","./test/data/nested"]
ls :: Monoid r => Acting IO r FilePath [FilePath]
ls = recovering $ act (\fp -> (fmap (fp </>)) <$> listDirectory fp)

-- | Fold over all files in the given directory.
-- If the focused path isn't a directory this fold will return 0 results
-- This is an alias for @@ls . traversed@@
--
-- >>> "./test/data" ^!! ls'ed
-- ["./test/data/flat","./test/data/symlinked","./test/data/.dotfile","./test/data/permissions","./test/data/nested"]
ls'ed :: Monoid r => Acting IO r FilePath FilePath
ls'ed = ls . traversed


-- | Append a path the end of the current path.
-- This uses `</>` for cross platform compatibility so
-- you don't need leading/trailing slashes here
--
-- >>> "./src" ^! path "Control"
-- "./src/Control"
path :: FilePath -> Getter FilePath FilePath
path filePath = to (</> filePath)

-- | Create a filepath from a list of path segments, then append it to the focused path.
--
-- >>> "." ^! pathL ["a", "b", "c"]
-- "./a/b/c"
pathL :: [FilePath] -> Getter FilePath FilePath
pathL filePaths = to (</> joinPath filePaths)

-- | "Branch" a fold into many sub-paths.
-- E.g. if we want to crawl into BOTH of @src@ and @test@ directories we can do:
--
-- >>> "." ^!! branching ["src", "test"] . ls
-- [["./src/Control"],["./test/Spec.hs","./test/data"]]
branching :: [FilePath] -> Fold FilePath FilePath
branching filePaths = folding (\fp -> (fp </>) <$> filePaths)

-- | Filter for only paths which point to a valid directory
--
-- >>> "./test" ^!! ls'ed
-- ["./test/Spec.hs","./test/data"]
--
-- >>> "./test" ^!! ls'ed . dirs
-- ["./test/data"]
dirs :: (Monoid r) => Acting IO r FilePath FilePath
dirs = filteredM doesDirectoryExist

-- | Filter for only paths which point to a valid file
--
-- >>> "./test" ^!! ls'ed
-- ["./test/Spec.hs","./test/data"]
--
-- >>> "./test" ^!! ls'ed . files
-- ["./test/Spec.hs"]
files :: (Monoid r) => Acting IO r FilePath FilePath
files = filteredM doesFileExist

-- | Get the contents of a file
-- This fold will return 0 results if the path does not exist, if it isn't a file, or if
-- reading the file causes any exceptions.
--
-- This fold lifts the path of the current file into the index of the fold in case you need it
-- downstream.
--
-- >>> "./test/data/flat/file.md" ^! contents
-- "markdown\n"
--
-- >>> "./test/data/flat/file.md" ^! contents . withIndex
-- ("./test/data/flat/file.md","markdown\n")
contents :: (Indexable FilePath p, Effective IO r f, Monoid r) => Over' p f FilePath String
contents = recovering (iact go)
  where
    go fp = do
        contents' <- readFile fp
        return (fp, contents')

-- | Filter the fold for only files which have ANY of the given file extensions.
-- E.g. to find all Haskell or Markdown files in the current directory:
--
-- >>> "./test/" ^!! crawled . exts ["hs", "md"]
-- ["./test/Spec.hs","./test/data/flat/file.md","./test/data/symlinked/file.md"]
exts :: [String] -> Traversal' FilePath FilePath
exts extList = filtered check
  where
    check fp = drop 1 (takeExtension fp) `elem` extList

-- | Crawl over every file AND directory in the given path.
--
-- >>> "./test/data/nested/top" ^!! crawled
-- ["./test/data/nested/top","./test/data/nested/top/mid","./test/data/nested/top/mid/bottom","./test/data/nested/top/mid/bottom/floor.txt"]
crawled :: Monoid r => Acting IO r FilePath FilePath
crawled = including (dirs . ls . traversed . crawled)

-- | Continually run the given fold until all branches hit dead ends,
-- yielding over all elements encountered the way.
--
-- >>> "./test/data" ^!! crawling (ls'ed . filtered ((== "flat") . view filename))
-- ["./test/data","./test/data/flat"]
crawling :: Monoid r => Acting IO r FilePath FilePath -> Acting IO r FilePath FilePath
crawling fld = including (recovering (fld . crawling fld))

-- | Make filepaths absolute in reference to the current working directory
--
-- > >>> "./test/data" ^! absolute
-- > "/Users/chris/dev/lens-filesystem/test/data"
absolute :: MonadicFold IO FilePath FilePath
absolute = act makeAbsolute

-- | Filter for only paths which have ALL of the given file-permissions
-- See 'readable', 'writable', 'executable'
--
-- >>> "./test/data" ^!! crawled . withPerms [readable, executable]
-- ["./test/data/permissions/exe"]
withPerms :: Monoid r => [Permissions -> Bool] -> Acting IO r FilePath FilePath
withPerms permChecks = filteredM checkAll
  where
    checkAll fp = do
        perms <- getPermissions fp
        return $ all ($ perms) permChecks

-- | If the path is a symlink, rewrite the path to its destination and keep folding
-- If it's not a symlink; pass the path onwards as is.
--
-- >>> "./test/data/symlinked" ^! symLinksFollowed
-- "flat"
symLinksFollowed :: Monoid r => Acting IO r FilePath FilePath
symLinksFollowed = tryOrContinue (act getSymbolicLinkTarget)
