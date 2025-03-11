{ pkgs, ... }:
let
  bogglePackage = pkgs.callPackage ./default.nix { };
in
{
  systemd.services.boggle = {
    description = "boggle solver http server";
    serviceConfig = {
      ExecStart = "${bogglePackage}/bin/dictionary";
      Restart = "always";
      RestartSec = 1;
    };
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
  };

  systemd.services.boggle.enable = true;
  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [ 8081 ];
    };
  };
}