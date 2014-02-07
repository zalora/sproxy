{ src }:
{
  build = import (src + "/default.nix") { inherit src; };
}
