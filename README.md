# ROLA
[![Build Status](https://travis-ci.com/appositum/rola.svg?branch=master)](https://travis-ci.com/appositum/rola)
<br>
<b>R</b>eally <b>O</b>utstanding <b>LA</b>mbda interpreter

### A few ways to build this
- **Nix**
```bash
nix run -c rola-exe
```

- **Stack**
```bash
stack install
stack build
stack exec rola-exe
```

- **Cabal**
```bash
cabal new-update
cabal new-build
cabal new-exec rola-exe
```
