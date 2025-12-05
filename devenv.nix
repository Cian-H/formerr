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
    pkgs.fypp
    pkgs.jinja2-cli
  ];

  languages.fortran.enable = true;

  treefmt = {
    enable = true;
    config.programs = {
      alejandra.enable = true;
      fprettify = {
        enable = true;
        excludes = ["codegen/templates/*"];
      };
      ruff-format.enable = true;
    };
  };

  git-hooks.hooks = {
    alejandra.enable = true;
    check-toml.enable = true;
    markdownlint.enable = true;
    mdformat.enable = true;
    taplo.enable = true;
    yamlfmt.enable = true;
  };

  scripts = {
    codegen.exec = "python $DEVENV_ROOT/codegen/generate_code.py && treefmt";
    build-dev.exec = "codegen && fortran-fpm build";
    build.exec = "codegen && fortran-fpm build --profile release";
    bench.exec = "build && fortran-fpm test bench --profile release";
    test-dev.exec = "build-dev && fortran-fpm test test";
    test-release.exec = "build && fortran-fpm test test --profile release";
  };
}
