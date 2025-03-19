{ pkgs ? import <nixpkgs> { }, ... }:
let pythonDeps = pkgs.python3.withPackages (ps: with ps; [ inflect nltk ]);
in pkgs.haskellPackages.shellFor {
  packages = hpkgs: [
    (import ./default.nix { })
  ];
  nativeBuildInputs = with (pkgs.haskellPackages); [
    haskell-language-server
    cabal-install
    stylish-haskell
    pythonDeps
  ];
}
