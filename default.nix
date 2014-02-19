{ pkgs ? import <nixpkgs> {}
, src ? ./. # Eventually want to filter out ignores
, zalora-lib ? import <zalora-nix-lib> { inherit pkgs; }
}:

{
  build = zalora-lib.buildLocalCabal src "sproxy";
}
