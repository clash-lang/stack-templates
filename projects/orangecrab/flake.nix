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
          serialportSrc = pkgs.fetchFromGitHub {
            owner = "standardsemiconductor";
            repo = "serialport";
            rev = "ce42a5afebb55d2e2e84be7f5386d69c343e3942";
            sha256 = "sha256-oBG8DylFwzUu212AiNOg2x+D1jmI2hAvMJQT0F8wWlE=";
          };
          clashCompilerSrc = pkgs.fetchFromGitHub {
            owner = "clash-lang";
            repo = "clash-compiler";
            rev = "7bf85bfbdb6561c068f99ec5f346d1b5092a011b";
            sha256 = "sha256-uiXDG8S5eJrQvwetU0YlAKu1C5RDFoW/3/j77nM0lYw=";
          };

          inherit (pkgs.haskell.lib) dontCheck doJailbreak markUnbroken;
          overlay = final: prev: {
            clash-prelude = prev.callCabal2nix "clash-prelude"
              (clashCompilerSrc + "/clash-prelude") { };
            clash-prelude-hedgehog = prev.callCabal2nix "clash-prelude-hedgehog"
              (clashCompilerSrc + "/clash-prelude-hedgehog") { };
            clash-lib = prev.callCabal2nix "clash-lib"
              (clashCompilerSrc + "/clash-lib") { };
            clash-ghc = prev.callCabal2nix "clash-ghc"
              (clashCompilerSrc + "/clash-ghc") { };
            serialport = dontCheck (prev.callCabal2nix "serialport" serialportSrc { });
            orangecrab = final.callCabal2nix "orangecrab" ./. { };
          };
          myHsPkgs = pkgs.haskell.packages.ghc9101.extend overlay;
      in
      {
        devShells.default = myHsPkgs.shellFor {
          name = "GHC 9.10.1";
          packages = p: [ p.orangecrab ];
          inputsFrom = [];
          nativeBuildInputs =
            with pkgs; [
              gnumake yosys nextpnr trellis
            ] ++
            (with myHsPkgs; [ cabal-install ])
            ++ [ecpprog.defaultPackage.${system}]
          ;
        };
        packages.default = dontCheck myHsPkgs.orangecrab;
      });
}
