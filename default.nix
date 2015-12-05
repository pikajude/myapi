# wrapper around generated.nix to add some build specific stuff
{ system ? builtins.currentSystem, nixpkgs ? import <nixpkgs> { inherit system; }, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  devMode = pkgs.lib.inNixShell;

  haskellPackages_ = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackages_.override {
    overrides = self: super: {
      mkDerivation = expr: super.mkDerivation (expr // {
        enableLibraryProfiling = devMode;
      });
    };
  };

  bowerPackages = let env = pkgs.callPackage ./nixfiles/bower-packages-generated.nix {}; in
  pkgs.stdenv.mkDerivation {
    name = "bower-components";
    buildCommand = ''
      mkdir -p $out
      cd $out
      for pkg in ${env}/packages/*/*; do
        name="$(${pkgs.jq}/bin/jq -r .name $pkg/.bower.json)"
        ln -sv "$pkg" "$name"
      done
    '';
  };

  sass = pkgs.bundlerEnv {
    name = "sass";
    gemset = ./gemset.nix;
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
  };

  myNodePackages = pkgs.nodePackages.override {
    generated = ./nixfiles/node-packages-generated.nix;
    self = myNodePackages;
  };

  drv = with haskellPackages; pkgs.haskell.lib.overrideCabal (callPackage ./nixfiles/pkg.nix {}) (p: {
    src = ./.;
    configureFlags = pkgs.lib.optional devMode "-fdevelopment";
    executableHaskellDepends = p.executableHaskellDepends ++ pkgs.lib.optional devMode snap-loader-dynamic;
    buildTools = (p.buildTools or []) ++ (with myNodePackages; [
      coffee-script cssmin uglify-js
    ]) ++ [ sass ]
      ++ pkgs.lib.optionals devMode [
      cabal-install cabal2nix hlint stylish-haskell
      pkgs.bundix pkgs.bundler pkgs.postgresql
    ];
  });

in

  pkgs.stdenv.lib.overrideDerivation (if devMode then drv.env else drv) (p: {
    BOWER_COMPONENTS = bowerPackages;
  })
