{ config, pkgs, ... }:
let
  inherit (pkgs.lib) mkOption mkIf types;

  cfg = config.services.sproxy;

  configFile = pkgs.writeText "sproxy.conf" cfg.config;
in {
  options = {
    services.sproxy = {
      enable = mkOption {
        default = false;

        type = types.bool;

        description = "Enable the sproxy secure proxy";
      };

      package = mkOption {
        type = types.package;

        description = "The built sproxy package";
      };

      authTokenKeyFile = mkOption {
        type = types.path;

        description = "The path to a file containing the auth_token_key";
      };

      clientSecretFile = mkOption {
        type = types.path;

        description = "The path to a file containing the Google Cloud Console client secret";
      };

      config = mkOption {
        type = types.lines;

        description = "The sproxy configuration";
      };

      sslKey = mkOption {
        type = types.path;

        description = "The path to the ssl key";
      };
    };
  };

  config = mkIf cfg.enable {
    services.sproxy.config = ''
      client_secret_file: ${cfg.clientSecretFile}
      auth_token_key_file: ${cfg.authTokenKeyFile}
      ssl_key: ${cfg.sslKey}
    '';

    systemd.services.sproxy = {
      description = "Sproxy secure proxy";

      wantedBy = [ "multi-user.target" ];

      serviceConfig.ExecStart = "${cfg.package}/bin/sproxy --config=${configFile}";
    };
  };
}
