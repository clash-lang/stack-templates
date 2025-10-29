{
  description = "A flake enabling tooling for orangecrab";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ecpprog.url = "github:diegodiv/ecpprog";
  };
  outputs = { self, nixpkgs, flake-utils, ecpprog, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          inherit (pkgs.haskell.lib) dontCheck doJailbreak markUnbroken;
          overlay = final: prev: {
            {{name}} = final.callCabal2nix "{{name}}" ./. { };
          };
          myHsPkgs = pkgs.haskell.packages.ghc9101.extend overlay;
      in
      {
        devShells.default = myHsPkgs.shellFor {
          name = "GHC 9.10.1";
          packages = p: [ ];
          inputsFrom = [];
          nativeBuildInputs =
            with pkgs; [
              gnumake yosys nextpnr trellis
            ] ++ [ecpprog.defaultPackage.${system}]
          ;
        };
      });
}
