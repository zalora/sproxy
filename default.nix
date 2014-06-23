{ pkgs ? import <nixpkgs> {}
, src ? ./. # Eventually want to filter out ignores
}:

{
  build = pkgs.haskellPackages.buildLocalCabalWithArgs {
    inherit src;
    name = "sproxy";
    cabalDrvArgs = {
        postInstall = ''
            source ${pkgs.makeWrapper}/nix-support/setup-hook
            wrapProgram $out/bin/sproxy \
            --prefix LD_LIBRARY_PATH : ${pkgs.stdenv.gcc.gcc}/lib64
        '';
    };
  };
}
