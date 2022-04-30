
{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  nativeBuildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
        containers
        witch
        QuickCheck
        hspec
        # hspec-discover
      ]))
      pkgs.haskellPackages.brittany
      pkgs.watchexec
      pkgs.hlint
      pkgs.ghcid
    ];
  }
