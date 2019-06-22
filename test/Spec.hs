{-# LANGUAGE LambdaCase #-}
import Control.Lens
import Control.Lens.Action
import Control.Lens.FileSystem
import System.Directory
import Test.Hspec
import Data.List

baseDir :: FilePath
baseDir = "test" </> "data"

main :: IO ()
main = do
    absRoot <- makeAbsolute baseDir
    setCurrentDirectory baseDir
    hspec $ do
      describe "ls" $ do
        it "should return files and dirs with full path" $ do
          sort <$> "flat" ^! ls `shouldReturn` ["flat/dir","flat/file.md","flat/file.txt"]
        it "should allow traversing deeper" $ do
          sort <$> "flat" ^!! ls . traversed `shouldReturn` ["flat/dir","flat/file.md","flat/file.txt"]
      describe "ls'ed" $ do
        it "should behave like 'ls' but with traversal" $ do
          sort <$> "flat" ^!! ls'ed `shouldReturn` ["flat/dir","flat/file.md","flat/file.txt"]
      describe "path" $ do
        it "should add to path" $ do
          "nested" ^! path "top" `shouldReturn` "nested/top"
        it "should add deep paths to path" $ do
          "nested" ^! path ("top" </> "mid" </> "bottom") `shouldReturn` "nested/top/mid/bottom"
      describe "pathL" $ do
        it "should add to path" $ do
          "nested" ^! pathL ["top"] `shouldReturn` "nested/top"
        it "should add deep pathL to path" $ do
          "nested" ^! pathL ["top", "mid", "bottom"] `shouldReturn` "nested/top/mid/bottom"
      describe "branching" $ do
        it "should follow many paths" $ do
          sort <$> "nested" ^! branching ["top", "peak"] . ls `shouldReturn`
            ["nested/peak/base","nested/peak/trees.txt","nested/top/mid"]
      describe "dirs" $ do
        it "should filter to only dirs" $ do
          "flat" ^!! ls . traversed . dirs `shouldReturn` ["flat/dir"]
      describe "files" $ do
        it "should filter to only files" $ do
          sort <$> "flat" ^!! ls . traversed . files `shouldReturn` ["flat/file.md", "flat/file.txt"]
      describe "contents" $ do
        it "should get file contents" $ do
          sort <$> "flat" ^!! ls . traversed . files . contents `shouldReturn` ["markdown\n", "text\n"]
      describe "exts" $ do
        it "should filter by extension" $ do
          sort <$> "flat" ^!! ls . traversed . exts ["", "txt"] `shouldReturn` ["flat/dir", "flat/file.txt"]
      describe "crawled" $ do
        it "should find ALL files and dirs under root including root" $ do
          sort <$> "nested" ^!! crawled `shouldReturn`
            ["nested","nested/peak","nested/peak/base","nested/peak/base/basecamp.txt"
            , "nested/peak/trees.txt","nested/top","nested/top/mid","nested/top/mid/bottom"
            , "nested/top/mid/bottom/floor.txt"]
      describe "absoluted" $ do
        it "should make paths absolute" $ do
          sort <$> "flat" ^!! ls . traversed . absolute `shouldReturn`
            [ absRoot </> "flat" </> "dir"
            , absRoot </> "flat" </> "file.md"
            , absRoot </> "flat" </> "file.txt"
            ]
      describe "withPerms" $ do
        it "should filter based on permissions" $ do
          "permissions" ^!! ls . traversed . withPerms [executable] `shouldReturn`
            ["permissions/exe" ]
          sort <$> "permissions" ^!! ls . traversed . withPerms [readable] `shouldReturn`
            ["permissions/exe", "permissions/readonly"]
        it "should 'and' permissions together" $ do
          "permissions" ^!! ls . traversed . withPerms [executable, readable, writable] `shouldReturn`
            ["permissions/exe" ]

      describe "symLinksFollowed" $ do
        it "should rewrite symlinks" $ do
          pendingWith "Need to look into portability, failing on CI"
          "symLinked" ^!! symLinksFollowed `shouldReturn` ["flat"]

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
