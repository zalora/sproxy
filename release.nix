args@{ nixpkgs, src }:
{
  build = import (src + "/default.nix") args;
}
