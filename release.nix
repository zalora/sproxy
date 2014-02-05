args@{ nixpkgs, src, officialRelease }:
{
  build = import (src + "/default.nix") args;
}
