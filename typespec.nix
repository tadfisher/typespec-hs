{ mkDerivation, alex, array, base, containers, Earley, filepath
, generic-lens, happy, hspec-discover, insert-ordered-containers
, lens, lib, megaparsec, mtl, parser-combinators, prettyprinter
, prettyprinter-ansi-terminal, safe-exceptions, scientific
, string-interpolate, template-haskell, text, unordered-containers
}:
mkDerivation {
  pname = "typespec";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    array base containers Earley filepath generic-lens
    insert-ordered-containers lens megaparsec mtl parser-combinators
    prettyprinter prettyprinter-ansi-terminal safe-exceptions
    scientific string-interpolate template-haskell text
    unordered-containers
  ];
  libraryToolDepends = [ alex happy hspec-discover ];
  homepage = "https://github.com/MercuryTechnologies/typespec-hs#readme";
  description = "Generate TypeSpec definitions from Haskell types";
  license = "unknown";
}
