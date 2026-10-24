{
  description = "A very basic flake with a devShell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    tilia.url = "github:mrkkrp/tilia";
  };
  outputs = { nixpkgs, self, tilia, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          cabal-install
          ghc
          ghcid
          haskell-language-server
          just
          tilia.packages.${system}.default
          pre-commit
        ];
      };
    };
}
