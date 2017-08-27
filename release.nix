let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = 
          haskellPackagesNew: 
          haskellPackagesOld: 
          rec {
            hash =
              haskellPackagesNew.callPackage ./default.nix { };

            reflex =
              haskellPackagesNew.callPackage ./reflex.nix { };

            reflex-dom =
              haskellPackagesNew.callPackage ./reflex-dom.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { hash = pkgs.haskellPackages.hash;
  }
