# Installation

```bash
$ cabal sandbox init                   # Initialise the sandbox
$ cabal install --only-dependencies    # Install dependencies into the sandbox
$ cabal build                          # Build your package inside the sandbox
```
# Testing

In Vim:
```
:map ,r :!/Users/zakv/.cabal/bin/cabal run<CR>
```

From command line:
```
cabal run
```

