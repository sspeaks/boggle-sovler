{ pkgs ? import <nixpkgs> { }, ... }:
let
  boggle = pkgs.haskellPackages.callCabal2nix "dict" ./. { };
  overriddenBoggle = boggle.overrideAttrs (oldAttrs: {
    # Add your additional files here like README, LICENSE, etc.
    extraFiles = [ "./updated_dictionary_keys.txt" ];

    # Modify the installPhase to include copying extra files
    installPhase = ''
      ${oldAttrs.installPhase}
      cp ./updated_dictionary_keys.txt $out/bin
    '';
  });

in
overriddenBoggle
