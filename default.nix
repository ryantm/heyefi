{ mkDerivation, base, bytestring, case-insensitive, configurator
, containers, directory, filepath, HandsomeSoup, hspec, hspec-wai
, http-types, hxt, iso8601-time, MissingH, multipart
, silently, stdenv, stm, tar, text, time, unix
, unordered-containers, utf8-string, wai, warp, ghc, cabal-install
, temporary, mtl, transformers, exceptions, random, hlint
, optparse-applicative, packunused
}:

mkDerivation {
  pname = "heyefi";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildTools = [ ghc cabal-install hlint packunused];
  buildDepends = [
    base bytestring case-insensitive configurator HandsomeSoup
    http-types hxt iso8601-time MissingH multipart stm tar
    text time unix unordered-containers utf8-string wai warp
    temporary filepath mtl transformers exceptions random
    optparse-applicative
  ];
  testDepends = [
    base bytestring case-insensitive configurator containers directory
    filepath HandsomeSoup hspec hspec-wai http-types hxt iso8601-time
    MissingH multipart silently stm tar text time unix
    unordered-containers utf8-string wai warp
    temporary mtl transformers exceptions random
    optparse-applicative
  ];
  homepage = "https://github.com/ryantm/heyefi";
  description = "A server for Eye-Fi SD cards written in Haskell. This project is not endorsed by Eye-Fi Inc.";
  license = stdenv.lib.licenses.publicDomain;
}
