{ sources ? import ./sources.nix }:

let
  overlay = _: pkgs: {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (pkgs) lib; };

    # Haskell overrides
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        # Add overrides here
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
