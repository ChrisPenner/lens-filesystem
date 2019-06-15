# lens-fs

examples:

```haskell
-- Find all files in ~ or ~/config with a .vim or .conf extension
>>> "/Users/chris" ^!! including (path "config") . ls . traversed . exts ["vim", "conf"]
["/Users/chris/.vim","/Users/chris/tmux.conf","/Users/chris/config/plugins.vim"]

-- Check whether a filename is a dotfile
>>> let isDotfile = has (filename . _head . only '.')
-- Crawl a filetree according to a given fold, 
-- e.g. crawl all dirs that aren't dotfiles (a.k.a. .git, .stack-work)
>>> "." ^!! crawling (ls'ed . dirs . filtered (not . isDotfile))
[ "." , "./app" , "./test" , "./src" , "./src/Control"
, "./src/Control/Lens", "./src/Control/Lens/FileSystem"]

-- Crawl ALL files in "src" collecting "*.hs" files, then make file paths absolute
>>> "src" ^!! crawled . exts ["hs"] . absolute
[ "/Users/chris/dev/lens-fs/src/Control/Lens/FileSystem/Combinators.hs"
, "/Users/chris/dev/lens-fs/src/Control/Lens/FileSystem.hs" ]

-- Find all executables in the 'scripts' directory and copy them to bin
>>> "scripts" & crawled . withPerms [executable] %! (`copyFile` "/Users/chris/bin")
```
