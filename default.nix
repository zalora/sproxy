{ pkgs ? import <nixpkgs> {}
, src ? ./. # Eventually want to filter out ignores
}:

let wrap = ''
      source ${pkgs.makeWrapper}/nix-support/setup-hook
      wrapProgram $out/bin/sproxy \
      --prefix LD_LIBRARY_PATH : ${pkgs.stdenv.gcc.gcc}/lib64
    '';
in {
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
}
