{ pkgs ? import <nixpkgs> {}
, src ? ./. # Eventually want to filter out ignores
}:

{
  build = pkgs.haskellPackages.buildLocalCabal src "sproxy";
}
