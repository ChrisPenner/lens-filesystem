{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Standard where

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

contents :: Action IO FilePath String
contents = act readFile

file :: FilePath -> Getter FilePath FilePath
file filePath = to (</> filePath)

dir :: FilePath -> Getter FilePath FilePath
dir = file

ls :: MonadicFold IO FilePath [FilePath]
ls = act listDirectory

mv :: FilePath -> MonadicFold IO FilePath ()
mv filePath = act (flip renamePath filePath)


infixr 8 !%~
(!%~) :: MonadicFold m s a -> (a -> m b) -> MonadicFold m s b
action !%~ f = action . act f

-- infixr 8 &!
(&!) :: (Monoid b, Monad m) => s -> MonadicFold m s b -> m b
v &! l = v ^! l

infixr 8 &!!
(&!!) :: (Monad m) => s -> MonadicFold m s b -> m [b]
v &!! l = v ^!! l

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


main :: IO ()
main = do
    fileContents <- "." ^!! file "README.md" . contents . lined
    -- print fileContents
    -- "." ^!! file "README.md" . contents . lined !%~ print
    "." &!! file "README.md" . contents . lined !%~ pure
    -- dirContents <- "." ^!! ls . traversed . filteredM doesDirectoryExist
    -- print dirContents
    return ()
