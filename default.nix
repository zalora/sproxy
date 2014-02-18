{ pkgs ? import <nixpkgs> {}
, src ? ./. # Eventually want to filter out ignores
, zalora-nix-lib ? import <zalora-nix-lib> { inherit pkgs; }
}:

{
  build = zalora-nix-lib.buildLocalCabal src "sproxy";
}
