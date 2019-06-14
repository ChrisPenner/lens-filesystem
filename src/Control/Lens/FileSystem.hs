{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Lens.FileSystem
    ( ls
    , path
    , path'
    , branching
    , dirs
    , files
    , contents
    , exts
    , crawled
    , absolute
    , withPerms
    , (</>)
    ) where

import Control.Lens
import Control.Lens.Action
import Control.Lens.FileSystem.Combinators
import System.Directory
import System.FilePath.Posix

ls :: MonadicFold IO FilePath [FilePath]
ls = act (\fp -> (fmap (fp </>)) <$> listDirectory fp)

path :: FilePath -> Getter FilePath FilePath
path filePath = to (</> filePath)

path' :: [FilePath] -> Getter FilePath FilePath
path' filePaths = to (</> joinPath filePaths)

branching :: [FilePath] -> Fold FilePath FilePath
branching filePaths = folding (\fp -> (fp </>) <$> filePaths)

dirs :: (Monoid r) => Acting IO r FilePath FilePath
dirs = filteredM doesDirectoryExist

files :: (Monoid r) => Acting IO r FilePath FilePath
files = filteredM doesFileExist

contents :: Action IO FilePath String
contents = act readFile

exts :: Monoid r => [String] -> Acting IO r FilePath FilePath
exts extList = filtered check
  where
    check fp = drop 1 (takeExtension fp) `elem` extList

crawled :: Monoid r => Acting IO r FilePath FilePath
crawled = unioned (recovering (ls . traversed . crawled))

absolute :: MonadicFold IO FilePath FilePath
absolute = act makeAbsolute

withPerms :: Monoid r => [Permissions -> Bool] -> Acting IO r FilePath FilePath
withPerms permChecks = filteredM checkAll
  where
    checkAll fp = do
        perms <- getPermissions fp
        return $ all ($ perms) permChecks

symLinksFollowed :: Monoid r => Acting IO r FilePath FilePath
symLinksFollowed = tryCatch (act getSymbolicLinkTarget) pure

localized :: (Conjoined p, Effective IO r f) => IO a -> Optic' p f FilePath a
localized action = act (flip withCurrentDirectory action)


