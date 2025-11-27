{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  packages = [
    pkgs.gfortran
    pkgs.git
    pkgs.fortls
    pkgs.fortran-fpm
    pkgs.fprettify
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

  scripts.run-bench.exec = "fortran-fpm test bench --profile release";
}
