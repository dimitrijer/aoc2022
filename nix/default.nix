{ sources ? import ./sources.nix
, config ? { }
, overlays ? [ ]
}:

import sources.nixpkgs {
  overlays = overlays ++ [ (import ./overlays.nix) ];
  config = config;
}
