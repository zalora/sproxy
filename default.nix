{ nixpkgs ? <nixpkgs>
, src ? ./. # Eventually want to filter out ignores
, officialRelease ? false
}:
let
  pkgs = import nixpkgs {};

  inherit (pkgs) haskellPackages lib;

  # Following to be moved into a library once we build more than
  # one package with nix
  cabalExpr = pkgs.runCommand "sproxy.nix" {} ''
    ${haskellPackages.cabal2nix}/bin/cabal2nix ${src + "/sproxy.cabal"} \
      --sha256=FILTERME | grep -v FILTERME > $out
  '';

  versionSuffix = if officialRelease
    then ""
    else "pre-${src.shortRev or "git"}";
in
  lib.overrideDerivation (haskellPackages.callPackage cabalExpr {
    tls = pkgs.haskellPackages.tls_1_1_5;
  }) (self: {
    name = self.name + versionSuffix;
    inherit src;
  })
