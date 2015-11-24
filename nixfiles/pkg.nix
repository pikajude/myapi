{ mkDerivation, base, blaze-html, bytestring, data-default, heist
, highlighter, http-types, lens, markdown
, MonadCatchIO-transformers, mtl, path-pieces, pcre-light
, persistent, persistent-template, snap, snap-core
, snap-loader-static, snap-server, snaplet-coffee
, snaplet-persistent, snaplet-sass, stdenv, text, time
, transformers, xmlhtml
}:
mkDerivation {
  pname = "myapi";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html bytestring data-default heist highlighter
    http-types lens markdown MonadCatchIO-transformers mtl path-pieces
    pcre-light persistent persistent-template snap snap-core
    snap-loader-static snap-server snaplet-coffee snaplet-persistent
    snaplet-sass text time transformers xmlhtml
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.unfree;
}
