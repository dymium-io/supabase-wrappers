{ mkDerivation, base, byteable, bytestring, cryptohash, exceptions, fgl
, file-embed, foldl, hashable, hasql, hasql-pool, hasql-th, hasql-transaction
, Interpolation, lens, lib, megaparsec, mtl, optparse-applicative, optparse-text
, path, path-io, text, time, unordered-containers, vector }:
mkDerivation {
  pname = "mallard";
  version = "0.6.3.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    byteable
    bytestring
    cryptohash
    exceptions
    fgl
    file-embed
    foldl
    hashable
    hasql
    hasql-pool
    hasql-th
    hasql-transaction
    Interpolation
    lens
    megaparsec
    mtl
    path
    path-io
    text
    unordered-containers
    vector
  ];
  executableHaskellDepends = [
    base
    exceptions
    fgl
    hasql
    hasql-pool
    lens
    mtl
    optparse-applicative
    optparse-text
    path
    path-io
    text
    time
    unordered-containers
  ];
  configureFlags = [ "--disable-shared" ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/AndrewRademacher/mallard";
  description = "Database migration and testing as a library";
  license = lib.licenses.mit;
  mainProgram = "mallard";
}
