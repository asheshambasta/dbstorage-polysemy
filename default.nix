{ system ? builtins.currentSystem }:
let
  sources = import ./nix/sources.nix;
  rp = import sources.reflex-platform { inherit system; };
in rp.project ({ pkgs, ... }: {

  name = "dbstorage-polysemy";

  # uses jsaddle-warp to fire up a warp server instead of webgtk.
  useWarp = true;
  withHoogle = false;

  packages = {
    dbstorage-polysemy = ./.;
  };

  shells = {
    ghc = [ "dbstorage-polysemy" ];
  }; 
  overrides = self: super: 
    with sources; { 
      prelude-polysemy = self.callCabal2nix "prelude-polysemy" prelude-polysemy {};
    };
})
