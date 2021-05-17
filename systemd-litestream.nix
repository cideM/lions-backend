{ config, lib, pkgs, ... }:
let
  makeLitestreamConfig = pkgs.writeShellScriptBin "make-litestream-config" ''
    cat << EOF > "$1"
    access-key-id: $(cat "$3/aws_key")
    secret-access-key: $(cat "$3/aws_secret")

    dbs:
      - path: "$2"
        replicas:
          - url: s3://lions-achern-litestream-replica-1/prod
    EOF

    /run/current-system/sw/bin/litestream replicate -config "$1"
  '';
in
with lib;
{

  config = {
    systemd.services.litestream = {

      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Group = "lions";
        ProtectSystem = "strict";
        ProtectHome = "yes";
        PrivateDevices = "yes";
        DynamicUser = "yes";
        User = "lions-server";
        PrivateTmp = "yes";
        RuntimeDirectory = "litestream";
        StateDirectory = "lions-server";
        ReadWritePaths = [
          "%S/lions-server/"
        ];
        LoadCredential = [
          "aws_key:/run/secrets/litestream_aws_key"
          "aws_secret:/run/secrets/litestream_aws_secret"
        ];
        ExecStart = "${makeLitestreamConfig}/bin/make-litestream-config %t/litestream/config.yaml %S/lions-server/db \${CREDENTIALS_DIRECTORY}";
        Restart = "always";
      };
    };
  };
}

