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

  scripts = {
    codegen.exec = "python $DEVENV_ROOT/tools/generate.py";
    build-dev.exec = "codegen && fortran-fpm build";
    build.exec = "codegen && fortran-fpm build --profile release";
    bench.exec = "codegen && build && fortran-fpm test bench --profile release";
    test-dev.exec = "codegen && build-dev && fortran-fpm test";
    test-release.exec = "codegen && build && fortran-fpm test --profile release";
  };
}
