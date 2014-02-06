args@{ nixpkgs, src, zalora-nix-lib }:
{
  build = import (src + "/default.nix") args;
}
