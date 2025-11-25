{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  packages = [
    pkgs.git
    pkgs.fortls
    pkgs.fortran-fpm
  ];

  languages.fortran.enable = true;

  git-hooks.hooks = {
    alejandra.enable = true;
    check-toml.enable = true;
    markdownlint.enable = true;
    mdformat.enable = true;
    taplo.enable = true;
    yamlfmt.enable = true;
  };
}
