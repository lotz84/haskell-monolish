# haskell-monolish

## Build with nix

```sh
$ NIXPKGS_ALLOW_UNFREE=1 nix-shell --run zsh
% stack build --ghc-options='-j4' --extra-lib-dirs=$MONOLISH_DIR/lib --extra-include-dirs=$MONOLISH_DIR/include .
% stack run --extra-lib-dirs=$MONOLISH_DIR/lib --extra-include-dirs=$MONOLISH_DIR/include .
```
