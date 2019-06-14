{-# LANGUAGE LambdaCase #-}
import Control.Lens
import Control.Lens.Action
import Control.Lens.FileSystem
import System.Directory
import Test.Hspec
import Data.Char

baseDir :: FilePath
baseDir = "test" </> "data"

main :: IO ()
main = do
    absRoot <- makeAbsolute baseDir
    setCurrentDirectory baseDir
    hspec $ do
      describe "ls" $ do
        it "should return files and dirs with full path" $ do
          "flat" ^! ls `shouldReturn` ["flat/file.txt","flat/file.md","flat/dir"]
        it "should allow traversing deeper" $ do
          "flat" ^!! ls . traversed `shouldReturn` ["flat/file.txt","flat/file.md","flat/dir"]
      describe "path" $ do
        it "should add to path" $ do
          "nested" ^! path "top" `shouldReturn` "nested/top"
        it "should add deep paths to path" $ do
          "nested" ^! path ("top" </> "mid" </> "bottom") `shouldReturn` "nested/top/mid/bottom"
      describe "path'" $ do
        it "should add to path" $ do
          "nested" ^! path' ["top"] `shouldReturn` "nested/top"
        it "should add deep path' to path" $ do
          "nested" ^! path' ["top", "mid", "bottom"] `shouldReturn` "nested/top/mid/bottom"
      describe "branching" $ do
        it "should follow many paths" $ do
          "nested" ^! branching ["top", "peak"] . ls `shouldReturn`
            ["nested/top/mid","nested/peak/trees.txt","nested/peak/base"]
      describe "dirs" $ do
        it "should filter to only dirs" $ do
          "flat" ^!! ls . traversed . dirs `shouldReturn` ["flat/dir"]
      describe "files" $ do
        it "should filter to only files" $ do
          "flat" ^!! ls . traversed . files `shouldReturn` ["flat/file.txt", "flat/file.md"]
      describe "contents" $ do
        it "should get file contents" $ do
          "flat" ^!! ls . traversed . files . contents `shouldReturn` ["text\n", "markdown\n"]
      describe "exts" $ do
        it "should filter by extension" $ do
          "flat" ^!! ls . traversed . exts ["", "txt"] `shouldReturn` ["flat/file.txt", "flat/dir"]
      describe "crawled" $ do
        it "should find ALL files and dirs under root including root" $ do
          "nested" ^!! crawled `shouldReturn`
            [ "nested", "nested/top", "nested/top/mid", "nested/top/mid/bottom"
            , "nested/top/mid/bottom/floor.txt", "nested/peak", "nested/peak/trees.txt"
            , "nested/peak/base", "nested/peak/base/basecamp.txt"]
      describe "absoluted" $ do
        it "should make paths absolute" $ do
          "flat" ^!! ls . traversed . absolute `shouldReturn`
            [ absRoot </> "flat" </> "file.txt"
            , absRoot </> "flat" </> "file.md"
            , absRoot </> "flat" </> "dir"
            ]
      describe "withPerms" $ do
        it "should filter based on permissions" $ do
          "permissions" ^!! ls . traversed . withPerms [executable] `shouldReturn`
            ["permissions/exe" ]
          "permissions" ^!! ls . traversed . withPerms [readable] `shouldReturn`
            ["permissions/readonly", "permissions/exe" ]
        it "should 'and' permissions together" $ do
          "permissions" ^!! ls . traversed . withPerms [executable, readable, writable] `shouldReturn`
            ["permissions/exe" ]
      describe "symLinksFollowed" $ do
        it "should rewrite symlinks" $ do
          "symLinked" ^!! symLinksFollowed `shouldReturn` ["flat"]
      describe "localized" $ do
        it "should run actions in a given dir" $ do
            "flat" ^! localized getCurrentDirectory `shouldReturn` absRoot </> "flat"


      describe "!%=" $ do
        it "should run an action over results, folding them together" $ do
          ("." & branching ["flat", "nested"] !%= pure . (:[]) . (fmap toUpper))
            `shouldReturn` ["./FLAT","./NESTED"]
      describe "!!%=" $ do
        it "should run an action over results, collecting them in a list" $ do
          ("." & branching ["flat", "nested"] !!%= pure . (fmap toUpper))
            `shouldReturn`["./FLAT","./NESTED"]

      describe "!%~" $ do
        it "should run an action over results, folding them together" $ do
          ("." & branching ["flat", "nested"] !%~ (:[]) . (fmap toUpper))
            `shouldReturn` ["./FLAT","./NESTED"]
      describe "!!%~" $ do
        it "should run an action over results, collecting them in a list" $ do
          ("." & branching ["flat", "nested"] !!%~ (fmap toUpper))
            `shouldReturn`["./FLAT","./NESTED"]

      describe "recovering" $ do
        it "should recover from exceptions with an empty fold" $ do
          [1 :: Int, 2, 3] ^!! traversed . recovering (act (\case 2 -> fail "nope"; n -> pure n))
            `shouldReturn` [1, 3]

      describe "tryOrContinue" $ do
        it "should return input when fold fails" $ do
          [1 :: Int, 2, 3] ^!! traversed . tryOrContinue (act (\case 2 -> fail "nope"; n -> pure (10*n)))
            `shouldReturn` [10, 2, 30]

      describe "tryCatch" $ do
        it "should recover from failure using handler" $ do
          [1 :: Int, 2, 3] ^!! traversed . tryCatch (act (\case 2 -> fail "nope"; n -> pure (10*n))) (pure . (*100))
            `shouldReturn` [10, 200, 30]

      describe "filteredM" $ do
        it "should filter out failing elements" $ do
          [1 :: Int, 2, 3] ^!! traversed . filteredM (pure . odd)
            `shouldReturn` [1, 3]

      describe "merging" $ do
        it "should combine elements from multiple folds" $ do
          [1 :: Int, 2, 3] ^!! traversed . merging (to (*10)) (to (*100))
            `shouldReturn` [10, 100, 20, 200, 30, 300]

      describe "including" $ do
        it "should add new elements while keeping old ones" $ do
          [1 :: Int, 2, 3] ^!! traversed . including (to (*10))
            `shouldReturn` [1, 10, 2, 20, 3, 30]
