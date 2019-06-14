import Control.Lens
import Control.Lens.Action
import Control.Lens.FileSystem
import System.Directory
import Test.Hspec

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




-- main :: IO ()
-- main = do
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
    -- "." ^! crawled . exts ["hs", "md"] . act print
    -- dirContents <- "." ^!! ls . traversed . filteredM doesDirectoryExist
    -- print dirContents
    -- return ()
