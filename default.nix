{ pkgs ? import <nixpkgs> {}
, src ? builtins.filterSource (path: type: let base = baseNameOf path; in
    type != "unknown" &&
    base != ".git" && base != "result" && base != "dist" && base != ".cabal-sandbox"
    ) ./.
}:

rec {
  buildExecutableOnly = pkgs.haskellPackages.callPackage (import ./sproxy.nix) {
    cabalDrvArgs = {
        postInstall = ''
            rm -rf $out/lib $out/share/doc
            source ${pkgs.makeWrapper}/nix-support/setup-hook
            wrapProgram $out/bin/sproxy --prefix LD_LIBRARY_PATH : ${pkgs.stdenv.gcc.gcc}/lib64
        '';
    };
  };

  sproxy = pkgs.stdenv.mkDerivation rec {
    inherit (buildExecutableOnly) version;
    name = "sproxy-static-${version}";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      install ${buildExecutableOnly}/bin/.sproxy-wrapped $out/bin/sproxy
    '';
  };
}
