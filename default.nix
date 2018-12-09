let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./rola.nix {}
