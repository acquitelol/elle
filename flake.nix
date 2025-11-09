{
  description = "Rust 1.88 dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    fenix.url = "github:nix-community/fenix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, fenix, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ fenix.overlays.default ];
        };
        rustToolchain = pkgs.fenix.fromToolchainFile {
          file = ./rust-toolchain.toml;
          sha256 = "sha256-Sei1CXOWypHP3ZUWVng7gWyBn+TcDV/0ThcocSKa9yk=";
        };
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            rustToolchain
          ];
        };
      });
}
