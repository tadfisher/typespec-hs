{
  description = "TypeSpec parser and code generator for Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }: let
    systems = nixpkgs.lib.systems.flakeExposed;

    haskells = ["ghcHEAD" "ghc96" "ghc98" "ghc910"];

    defaultHaskell = "ghc96";

    eachSystem = nixpkgs.lib.genAttrs systems;

    eachHaskell = nixpkgs.lib.genAttrs haskells;

    pkgsBySystem = eachSystem (system: nixpkgs.legacyPackages.${system});

    haskellPackages = eachSystem (
      system: let
        pkgs = pkgsBySystem.${system};
      in
        eachHaskell (haskell: pkgs.haskell.packages.${haskell})
    );
  in {
    packages = eachSystem (
      system: let
        packages' =
          nixpkgs.lib.mapAttrs'
          (n: v: {
            name = "typespec-${n}";
            value = v;
          })
          (eachHaskell (
            haskell:
              haskellPackages.${system}.${haskell}.callPackage ./typespec.nix {}
          ));
      in
        packages' // {default = packages'."typespec-${defaultHaskell}";}
    );

    devShells = eachSystem (
      system: let
        pkgs = pkgsBySystem.${system};

        shells = eachHaskell (
          haskell: let
            hsPkgs = haskellPackages.${system}.${haskell};
          in
            pkgs.mkShell {
              name = "typespec-hs-${haskell}-shell";
              inputsFrom = [self.packages.${system}."typespec-${haskell}"];
              nativeBuildInputs = [
                hsPkgs.cabal2nix
                hsPkgs.cabal-install
                hsPkgs.ghc
                hsPkgs.ghcid
                hsPkgs.haskell-language-server
                hsPkgs.hlint
                hsPkgs.hpack
                hsPkgs.fourmolu
                pkgs.nixfmt-rfc-style
              ];
              shellHook = ''
                hpack . && cabal2nix . > typespec.nix
              '';
            }
        );
      in
        shells // {default = shells.${defaultHaskell};}
    );
  };
}
