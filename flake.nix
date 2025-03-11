{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
  };
  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system}.pkgs;
    in
    {
      packages.${system}.default = pkgs.callPackage ./default.nix { };
      nixosModules.default = import ./boggleServer.nix;
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}