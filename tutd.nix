
with (import <nixpkgs> {}).lib;
let
  lib = import ./lib.nix;
  unique = (import <nixpkgs> {}).lib.lists.unique;
  #validStorePath = /. + "nix/store" + builtins.elemAt (builtins.split "/nix/store" "${import ./project-m36-mainfiles-rearranged.nix {}}") 2;
  whereCabalMacro = ignore ["result/"] ./.;
  ignore = paths: s: cleanSourceWith {
    filter = path: type: 
    let result = !(lists.elem (/. + path) paths);
    in result;
    src = s;
  };
  validStorePath = s: /. + "nix/store" + builtins.elemAt (builtins.split "/nix/store" s.outPath) 2;

in
  { main = "tutd";
    src = ../project-m36/src/bin; 
    packages = [lib];
    dependencies = unique (lib.dependencies ++ ["base" "HUnit" "Cabal" "containers" "hashable" "unordered-containers" "mtl" "vector" "vector-binary-instances" "time" "hashable-time" "bytestring" "uuid" "stm" "deepseq" "deepseq-generics" "binary" "parallel" "cassava" "attoparsec" "gnuplot" "directory" "temporary" "haskeline" "megaparsec" "text" "base64-bytestring" "base16-bytestring" "data-interval" "filepath" "stm-containers" "list-t" "random" "MonadRandom" "semigroups" "parser-combinators" "http-types" "http-api-data" "http-conduit" "modern-uri"]);
    extensions = ["OverloadedStrings" "CPP"];
    ghcOpts = ["-DPM36_HASKELL_SCRIPTING" 
               # "-prof" "-fprof-auto" "-rtsopts" 
               "-fexternal-interpreter" "-Wall" "-threaded" "-rdynamic"
             ];
  extra-directories =
    (modName: [../project-m36/cbits]) ;
  extra-files = (modName: [./cabal_macros.h]);
  }
