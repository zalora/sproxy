{ pkgs ? import <nixpkgs> {}
, src ? ./. # Eventually want to filter out ignores
}:

let wrap = ''
      source ${pkgs.makeWrapper}/nix-support/setup-hook
      wrapProgram $out/bin/sproxy \
      --prefix LD_LIBRARY_PATH : ${pkgs.stdenv.gcc.gcc}/lib64
    '';
in rec {
  build = pkgs.haskellPackages.buildLocalCabalWithArgs {
    inherit src;
    name = "sproxy";
    cabalDrvArgs = {
        postInstall = wrap;
    };
  };

  buildExecutableOnly = pkgs.haskellPackages.buildLocalCabalWithArgs {
    inherit src;
    name = "sproxy";
    cabalDrvArgs = {
        postInstall = ''
            rm -rf $out/lib $out/share/doc
            ${wrap}
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
