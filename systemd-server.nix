{ config, lib, ... }:
with lib;
{
  options = {
    server = mkOption {
      type = types.package;
    };
  };

  config = {
    systemd.services.server = {

      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Environment = "LIONS_SQLITE_PATH=%S/sqlite/db LIONS_ENV=production";
        ProtectSystem = "strict";
        ProtectHome = "yes";
        PrivateDevices = "yes";
        DynamicUser = "yes";
        PrivateTmp = "yes";
        StateDirectory = "sqlite";
        BindReadOnlyPaths = "/nix/store";
        WorkingDirectory = "${config.server}";
        ExecStart = "${config.server}/server";
        Restart = "on-failure";
      };
    };
  };
}
