# Rola
[![Build Status](https://travis-ci.com/appositum/rola.svg?branch=master)](https://travis-ci.com/appositum/rola)
Really Outstanding LAmbda interpreter

### A few ways to build this
- **Nix**
```bash
nix-build
./result/bin/rola-exe
```

- **Stack**
```bash
stack install
stack exec rola-exe
```

- **Cabal** (might not work on GHC versions other than 8.4.x)
```bash
cabal update
cabal new-run exe:rola
```
