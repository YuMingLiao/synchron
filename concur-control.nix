{ mkDerivation, aeson, amqp, async, base, bytestring, comonad
, containers, free, hashtables, hpack, http-types, list-zipper
, replica, lib, stm, svg-builder, tasty, tasty-hunit, text
, transformers, wai, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "concur-control";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amqp async base bytestring comonad containers free hashtables
    http-types list-zipper replica stm svg-builder text transformers
    wai wai-websockets warp websockets tasty tasty-hunit
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson amqp async base bytestring comonad containers free hashtables
    http-types list-zipper replica stm svg-builder text transformers
    wai wai-websockets warp websockets
  ];
  testHaskellDepends = [
    aeson amqp async base bytestring comonad containers free hashtables
    http-types list-zipper replica stm svg-builder tasty tasty-hunit
    text transformers wai wai-websockets warp websockets
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/concur-control#readme";
  license = lib.licenses.bsd3;
}
