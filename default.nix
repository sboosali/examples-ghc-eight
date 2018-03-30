{ mkDerivation, base, containers, either, generic-lens, haskeline
, lens, mtl, stdenv, text-zipper
}:
mkDerivation {
  pname = "examples-ghc-eight";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base either generic-lens lens mtl text-zipper
  ];
  executableHaskellDepends = [
    base containers either generic-lens haskeline lens mtl text-zipper
  ];
  homepage = "http://github.com/sboosali/examples-ghc-eight#readme";
  description = "simple examples for libraries, patterns, and extensions (GHC 8.2).";
  license = stdenv.lib.licenses.bsd3;
}
