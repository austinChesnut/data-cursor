
{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  nativeBuildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
        cursor
        aeson
        lens
        containers
        hoogle
        parsec
      ]))
      # pkgs.ormolu
      pkgs.haskellPackages.brittany
      pkgs.watchexec
      pkgs.graphviz
      # pkgs.cabal2nix
      # pkgs.cabal-install
      pkgs.hlint
      # (pkgs.haskellPackages.callPackage ./HsTodo.nix { })
      pkgs.ghcid
    ];
  }
