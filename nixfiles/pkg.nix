{ mkDerivation, base, blaze-builder, blaze-html, bytestring
, cryptohash, data-default, digestive-functors
, digestive-functors-heist, digestive-functors-snap, directory
, filepath, heist, highlighter, http-types, lens, markdown
, monad-logger, MonadCatchIO-transformers, mtl, path-pieces
, pcre-light, persistent, persistent-template, process-extras
, resourcet, snap, snap-core, snap-extras, snap-loader-static
, snap-server, snaplet-coffee, snaplet-persistent, snaplet-sass
, stdenv, template-haskell, text, time, transformers, true-name
, xmlhtml
}:
mkDerivation {
  pname = "myapi";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-builder blaze-html bytestring cryptohash data-default
    digestive-functors digestive-functors-heist digestive-functors-snap
    directory filepath heist highlighter http-types lens markdown
    monad-logger MonadCatchIO-transformers mtl path-pieces pcre-light
    persistent persistent-template process-extras resourcet snap
    snap-core snap-extras snap-loader-static snap-server snaplet-coffee
    snaplet-persistent snaplet-sass template-haskell text time
    transformers true-name xmlhtml
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.unfree;
}
