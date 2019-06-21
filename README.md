# lens-filesystem

[HACKAGE](https://hackage.haskell.org/package/lens-filesystem)

A lensy style interface to your filesystem.

This is pretty experimental; I wouldn't recommend using it in production code at the moment;
Using the read-only operations should be fine, but I'd strongly recommend doing lots of testing
with `print` before you run destructive filesystem operations.

This library is meant to be used in conjunction with the `lens-action` library.

The interface to this package could change at any time.

Examples:

```haskell
Many of the combinators you see here come from `lens-action`.

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
>>> "scripts" ^! crawled . withPerms [executable] . act (`copyFile` "/Users/chris/bin")

-- Read all markdown files and get their contents with filename
>>> "./test" ^!! crawled . exts ["md"] . contents . withIndex
[("./test/data/flat/file.md","markdown\n")]
```

See more examples in the [tests](./test/Spec.hs)
