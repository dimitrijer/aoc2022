{ pkgs ? import ./nix/default.nix { } }:

let
  sources = import ./nix/sources.nix;
  nixfiles = import sources.nixfiles { };
  neovim = nixfiles.neovim {
    pkgs = pkgs;
    withOCaml = true;
    withWriting = true;
  };
in
pkgs.mkShell
{
  shellHooks = ''
    alias vim='nvim'
  '';

  buildInputs = with pkgs; [
    nixpkgs-fmt
    fswatch # for dune build -w
    ocamlformat
    ocamlPackages.odoc
    ocamlPackages.ocaml-lsp
    ocamlPackages.utop
    ocamlPackages.dune_3
    ocamlPackages.ounit2
    ocamlPackages.ocaml
    ocamlPackages.findlib
    ocamlPackages.angstrom
    ocamlPackages.janeStreet.base
    ocamlPackages.janeStreet.core_extended
    ocamlPackages.janeStreet.stdio
    ocamlPackages.janeStreet.ppx_jane
  ] ++ [ neovim ];
}
