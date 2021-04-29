{ pkgs, config, ... }:

{
  config = {
    users.users.root.password = "";
    users.mutableUsers = false;
  };
}
