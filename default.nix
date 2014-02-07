{ pkgs ? import <nixpkgs> {}
, src ? ./. # Eventually want to filter out ignores
, zalora-nix-lib ? import <zalora-nix-lib> { inherit pkgs; }
}: (zalora-nix-lib.buildLocalCabal src "sproxy").override {
  tls = pkgs.haskellPackages.tls_1_1_5;
}
