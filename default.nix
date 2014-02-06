{ nixpkgs ? <nixpkgs>
, src ? ./. # Eventually want to filter out ignores
, zalora-nix-lib ? <zalora-nix-lib>
}:
let
  nix-lib = import zalora-nix-lib { inherit pkgs; };
  pkgs = import nixpkgs {};
in (nix-lib.buildLocalCabal src "sproxy").override {
  tls = pkgs.haskellPackages.tls_1_1_5;
}
