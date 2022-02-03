let
   hostPkgs = import <nixpkgs> {};
in
rec {
  ghc-version = "ghc8107";
  pkgs = hostPkgs; 
}
