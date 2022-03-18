{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
pkgs.mkShell {
  buildInputs = [
    # keep this line if you use bash
    pkgs.bashInteractive
    pkgs.ghc
    pkgs.ormolu
  ];
}
