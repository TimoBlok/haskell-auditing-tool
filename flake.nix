# {
#   inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
#   inputs.flake-utils.url = "github:numtide/flake-utils";
#   outputs = { self, nixpkgs, flake-utils }:
#     (flake-utils.lib.eachDefaultSystem (system:
#     let haskell_packages = nixpkgs.legacyPackages.${system}.haskell.packages; in
#     {
#       # Build an output for each version of GHC in the nixpkgs snapshot
#       packages = nixpkgs.lib.attrsets.genAttrs (nixpkgs.lib.attrNames haskell_packages) (ghc_version:
#         # Use 'cabal build' to compile the Haskell
#         # (but with GHC & Haskell libraries from the nixpkgs snapshot)
#         haskell_packages.${ghc_version}.developPackage {
#           root = ./.;
#         });
#     }));
# }

# nix
# IGNORE THIS FILE (and flake.lock) if you don't know what 'nix' is
# The recommended tool to build/test your code is cabal
 
# If you are a nix enhusiast,
#  you can use this file to get IDE integration
#  without messing up your global system state
 
# To do this, run `nix develop` in this directory,
#  then start your normal editor (e.g. `code .`)
 
# N.B. you are expected to install the editor yourself,
#   as well as set up any language-server plugins it needs
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
 
  outputs = {self, nixpkgs, flake-utils}:
    (flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      hpkgs = pkgs.haskellPackages;
      pkg = hpkgs.developPackage {root = ./.;}; # Pass the buck to cabal
    in {
      # What `nix build` should build
      packages.default = pkg;
      # What `nix develop` should prepare a dev env for
      devShells.default = hpkgs.shellFor {
          # Which haskell packages to prepare a dev env for
          packages = _: [pkg];
          # Provide extra software in the dev shell
          nativeBuildInputs = [
              pkgs.ghcid
              pkgs.cabal-install
              pkgs.haskell-language-server
            ];
          # Include an offline-usable `hoogle` command
          # pre-loaded with all the dependencies
          withHoogle = true;
        };
  }));
}
