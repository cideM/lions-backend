{ config, pkgs, lib, ... }:
with lib;
{
  config = {
    sops.secrets.litestream_aws_key = { };
    sops.secrets.litestream_aws_secret = { };
    sops.secrets.aws_ses_access_key = { };
    sops.secrets.aws_ses_secret_access_key = { };
    sops.secrets.signerkey = { };
    sops.secrets.saltsep = { };

    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };
}
