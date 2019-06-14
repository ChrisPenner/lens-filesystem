{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Lens.FileSystem where

import Control.Lens
import Control.Lens.Action
import Control.Lens.FileSystem.Combinators
import System.Directory
import System.FilePath.Posix

localized :: (Conjoined p, Effective IO r f) => IO a -> Optic' p f FilePath a
localized action = act (flip withCurrentDirectory action)

contents :: Action IO FilePath String
contents = act readFile

path :: FilePath -> Getter FilePath FilePath
path filePath = to (</> filePath)

paths :: [FilePath] -> Getter FilePath FilePath
paths filePaths = to (</> joinPath filePaths)

ls :: MonadicFold IO FilePath [FilePath]
ls = act (\fp -> (fmap (fp </>)) <$> listDirectory fp)

absoluted :: MonadicFold IO FilePath FilePath
absoluted = act makeAbsolute

dirs :: (Monoid r) => Acting IO r FilePath FilePath
dirs = filteredM doesDirectoryExist

files :: (Monoid r) => Acting IO r FilePath FilePath
files = filteredM doesFileExist

withPerms :: Monoid r => [Permissions -> Bool] -> Acting IO r FilePath FilePath
withPerms permChecks = filteredM checkAll
  where
    checkAll fp = do
        perms <- getPermissions fp
        return $ all ($ perms) permChecks

exts :: Monoid r => [String] -> Acting IO r FilePath FilePath
exts extList = filtered check
  where
    check fp = drop 1 (takeExtension fp) `elem` extList

symLinksFollowed :: Monoid r => Acting IO r FilePath FilePath
symLinksFollowed = tryCatch (act getSymbolicLinkTarget) pure

crawled :: Monoid r => Acting IO r FilePath FilePath
crawled = unioned (recovering (ls . traversed . crawled))

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
    -- Config "." ^! workDir . ls . traversed . symLinked . act print
    -- "." ^! path ("src" </> "Control" </> "Lens") . crawled . act print
    -- "." ^! path ("src" </> "Control") . crawled . dirs . localized getCurrentDirectory . act print
    "." ^! crawled . exts ["hs", "md"] . act print
    -- dirContents <- "." ^!! ls . traversed . filteredM doesDirectoryExist
    -- print dirContents
    return ()
