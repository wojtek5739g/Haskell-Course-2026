Haskell project managed by Windows version of GHCup: https://www.haskell.org/ghcup/

### Either from inside specific homework directory invoke:

Run Haskell REPL:
```
ghci
```
Load a file:
```
:l Solution.hs
```
and then invoke some loaded functions, e.g. type:
```
quicksort[3, 7, 5, 4]
```

### OR:

Put functions invokation inside main of .hs files and type, e.g.:

```
main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    print (quicksort [3, 6, 8, 10, 1, 2, 1])
```

compile manually:
```
ghc Solution.hs
```

and run:
```
./Solution.exe
```



