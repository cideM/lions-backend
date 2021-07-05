{ config, pkgs, lib, ... }:
with lib;
let
  migrateScript = pkgs.writeScript "lions-migrations" ''
    #!${pkgs.runtimeShell}
    ${pkgs.go-migrate}/bin/migrate -path ./backend/migrations -database "sqlite3://$LIONS_SQLITE_PATH" up
  '';
in
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
    systemd.services.migrations = {
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Environment = [
          "LIONS_SQLITE_PATH=%S/lions-server/db"
        ];
        ProtectSystem = "strict";
        Group = "lions";
        User = "lions-server";
        ProtectHome = "yes";
        PrivateDevices = "yes";
        Conflicts = "litestream.service";
        Before = "server.service";
        PrivateTmp = "yes";
        StateDirectory = "lions-server";
        WorkingDirectory = "${config.serverWorkingDir}";
        Restart = "on-failure";
        ExecStart = migrateScript;
      };
    };

    systemd.services.server = {
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Environment = [
          "LIONS_SQLITE_PATH=%S/lions-server/db"
          "LIONS_STORAGE_DIR=%S/lions-server/storage/"
          "LIONS_ENV=production"
          "LIONS_SESSION_KEY_FILE=%S/lions-server/sessionkey.aes"
        ];
        ProtectSystem = "strict";
        Group = "lions";
        User = "lions-server";
        ProtectHome = "yes";
        LoadCredential = [
          "aws_ses_access_key:/run/secrets/aws_ses_access_key"
          "aws_ses_secret_access_key:/run/secrets/aws_ses_secret_access_key"
          "signerkey:/run/secrets/signerkey"
          "saltsep:/run/secrets/saltsep"
        ];
        PrivateDevices = "yes";
        PrivateTmp = "yes";
        StateDirectory = "lions-server";
        WorkingDirectory = "${config.serverWorkingDir}";
        ExecStart = "/bin/sh -c \"export LIONS_AWS_SES_ACCESS_KEY=$(cat \${CREDENTIALS_DIRECTORY}/aws_ses_access_key) LIONS_AWS_SES_SECRET_ACCESS_KEY=$(cat \${CREDENTIALS_DIRECTORY}/aws_ses_secret_access_key) LIONS_SCRYPT_SIGNER_KEY=$(cat \${CREDENTIALS_DIRECTORY}/signerkey) LIONS_SCRYPT_SALT_SEP=$(cat \${CREDENTIALS_DIRECTORY}/saltsep); ${config.serverExe}\"";
        Restart = "on-failure";
      };
    };
  };
}
