{ mkDerivation, base, fingertree, primitive, QuickCheck, stdenv
, tasty, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "vector-rope";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base fingertree primitive vector ];
  testHaskellDepends = [
    base fingertree QuickCheck tasty tasty-quickcheck vector
  ];
  homepage = "https://github.com/esoeylemez/vector-rope";
  description = "Vectors with efficient appends and updates";
  license = stdenv.lib.licenses.bsd3;
}
