{ mkDerivation, base, bytestring, case-insensitive, configurator
, directory, exceptions, filepath, HandsomeSoup, hspec, hspec-wai
, http-types, hxt, iso8601-time, MissingH, mtl, multipart
, optparse-applicative, random, silently, stdenv, stm, tar
, temporary, text, time, transformers, unix, unordered-containers
, utf8-string, wai, wai-extra, warp
}:
mkDerivation {
  pname = "heyefi";
  version = "2.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring case-insensitive configurator directory exceptions
    filepath HandsomeSoup http-types hxt iso8601-time MissingH mtl
    multipart optparse-applicative random stm tar temporary text time
    transformers unix unordered-containers utf8-string wai warp
  ];
  testHaskellDepends = [
    base bytestring case-insensitive configurator directory exceptions
    filepath HandsomeSoup hspec hspec-wai http-types hxt iso8601-time
    MissingH mtl multipart optparse-applicative random silently stm tar
    temporary text time transformers unix unordered-containers
    utf8-string wai wai-extra warp
  ];
  homepage = "https://github.com/ryantm/heyefi";
  description = "A server for Eye-Fi SD cards";
  license = stdenv.lib.licenses.publicDomain;
}
