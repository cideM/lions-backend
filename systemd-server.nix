{ config, lib, ... }:
with lib;
{
  options = {
    serverExe = mkOption {
      type = types.path;
    };

    serverWorkingDir = mkOption {
      type = types.path;
    };
  };

  config = {
    systemd.services.server = {

      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Environment = [
          "LIONS_SQLITE_PATH=%S/lions-server/db"
          "LIONS_ENV=production"
          "LIONS_SESSION_KEY_FILE=%S/lions-server/sessionkey.aes"
        ];
        ProtectSystem = "strict";
        Group = "lions";
        User = "lions-server";
        ProtectHome = "yes";
        PrivateDevices = "yes";
        DynamicUser = "yes";
        PrivateTmp = "yes";
        StateDirectory = "lions-server";
        WorkingDirectory = "${config.serverWorkingDir}";
        ExecStart = "${config.serverExe}";
        Restart = "on-failure";
      };
    };
  };
}
