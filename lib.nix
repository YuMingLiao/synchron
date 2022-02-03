with (import <nixpkgs> {}).lib;
{ 
  src = ./src;
  "dependencies" = ["aeson" "amqp" "async" "base" "bytestring" "comonad" "containers" "free" "hashtables"
    "http-types" "list-zipper" "replica" "stm" "svg-builder" "text" "transformers"
      "wai" "wai-websockets" "warp" "websockets"
    ];
  extensions = []; 
  ghcOpts = [];
  extra-directories =
    (modName: []);
  extra-files = (modName: []);
}


