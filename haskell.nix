haskellPackageList:

let
  githubRepo = fq: rev:
    builtins.fetchTarball ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
  packageOverlay = self: super:
    {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: {
          relude = hsuper.callCabal2nix "relude" (githubRepo "kowainik/relude" "bfb5f60") {};
        };
      };
    };
  overlays = [packageOverlay];
  nixpkgsSrc = (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/f97746ba272.tar.gz");
  nixpkgs = import nixpkgsSrc { inherit overlays; };
in
  nixpkgs.haskellPackages.ghcWithPackages haskellPackageList
