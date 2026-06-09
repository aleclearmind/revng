{ self, inputs, system }:
let
  pkgsCtx = import ./pkgs.nix { inherit inputs system; };
  inherit (pkgsCtx) pkgs pkgs-2505 stdenv ccacheStdenv python;
  lib = pkgs.lib;

  scope = lib.makeScope pkgs.newScope (
    sp:
    let
      awsPkgs = sp.callPackage ./packages/aws-sdk-cpp.nix { };
      qemuPkgs = sp.callPackage ./packages/qemu.nix { };
      revngQaPkgs = sp.callPackage ./packages/revng-qa.nix { };
      modelsPkgs = sp.callPackage ./models.nix { };
      rootfsPkgs = sp.callPackage ./packages/rootfs.nix { };
      winRootfsPkgs = sp.callPackage ./packages/win-rootfs.nix { };
      win32metadataPkgs = sp.callPackage ./packages/win32metadata { };
    in
    {
      inherit
        pkgs
        pkgs-2505
        stdenv
        ccacheStdenv
        python
        lib
        system
        inputs
        ;
      revngPackages = sp;
      revngClang = pkgs-2505.clang_16;
      crossToolchains = import ./packages/cross-toolchains.nix {
        inherit (inputs) nixpkgs;
        inherit system;
      };
      msvc = sp.callPackage ./packages/msvc { };
      boost = sp.callPackage ./packages/boost.nix { };
      inherit (awsPkgs) aws-crt-cpp aws-sdk-cpp;
      revngPythonDependencies = sp.callPackage ./packages/revng-python-dependencies { };
      revngJavascriptDependencies = sp.callPackage ./packages/revng-js-dependencies { };
      llvm = sp.callPackage ./packages/llvm.nix { };
      clangRelease = sp.callPackage ./packages/clang-release.nix { };
      inherit (qemuPkgs) qemu qemuHelpers;
      nanobind = sp.callPackage ./packages/nanobind.nix { };
      ninjaShellRule = sp.callPackage ./packages/ninja { };
      inherit (revngQaPkgs) revng-qa;
      "test/revng-qa" = revngQaPkgs."test/revng-qa";
      revng-qa-idb = sp.callPackage ./packages/revng-qa-idb.nix { };
      "macos/sdk" = sp.callPackage ./packages/macos/sdk.nix { };
      "macos/libtapi" = sp.callPackage ./packages/macos/libtapi.nix { };
      "macos/ld64/x86-64" = (sp.callPackage ./packages/macos/ld64.nix { })."x86-64";
      "macos/ld64/i686" = (sp.callPackage ./packages/macos/ld64.nix { }).i686;
      "macos/ld64/arm" = (sp.callPackage ./packages/macos/ld64.nix { }).arm;
      "macos/ld64/aarch64" = (sp.callPackage ./packages/macos/ld64.nix { }).aarch64;
      "macos/clang/x86-64" = (sp.callPackage ./packages/macos/clang.nix { })."x86-64";
      "macos/clang/i686" = (sp.callPackage ./packages/macos/clang.nix { }).i686;
      "macos/clang/arm" = (sp.callPackage ./packages/macos/clang.nix { }).arm;
      "macos/clang/aarch64" = (sp.callPackage ./packages/macos/clang.nix { }).aarch64;
      revng = sp.callPackage ./packages/revng.nix { };
      inherit (modelsPkgs) mkModels;
      "test/revng-qa/models" = sp.callPackage ./packages/test-revng-qa-models.nix { };
      model-db = sp.callPackage ./packages/model-db.nix { };
      "test/revng" = sp.callPackage ./packages/test-revng.nix { };
      "test/revng-prss" = sp.callPackage ./packages/test-revng-prss.nix { };

      "rootfs/ubuntu-20-04-x86-64" = rootfsPkgs."rootfs/ubuntu-20-04-x86-64";
      "rootfs/ubuntu-22-04-x86-64" = rootfsPkgs."rootfs/ubuntu-22-04-x86-64";
      "rootfs/ubuntu-24-04-x86-64" = rootfsPkgs."rootfs/ubuntu-24-04-x86-64";
      "rootfs/ubuntu-24-04-i386" = rootfsPkgs."rootfs/ubuntu-24-04-i386";
      "rootfs/ubuntu-24-04-arm" = rootfsPkgs."rootfs/ubuntu-24-04-arm";
      "rootfs/ubuntu-24-04-aarch64" = rootfsPkgs."rootfs/ubuntu-24-04-aarch64";
      "rootfs/ubuntu-24-04-s390x" = rootfsPkgs."rootfs/ubuntu-24-04-s390x";
      "rootfs/debian-bookworm-mipsel" = rootfsPkgs."rootfs/debian-bookworm-mipsel";
      "rootfs/debian-buster-mips" = rootfsPkgs."rootfs/debian-buster-mips";

      "rootfs/windows-x86-64" = winRootfsPkgs."rootfs/windows-x86-64";
      "rootfs/windows-aarch64" = winRootfsPkgs."rootfs/windows-aarch64";
      "rootfs/windows-7-x86" = winRootfsPkgs."rootfs/windows-7-x86";
      "rootfs/windows-8-x86-64" = winRootfsPkgs."rootfs/windows-8-x86-64";
      "rootfs/windows-8-1-x86-64" = winRootfsPkgs."rootfs/windows-8-1-x86-64";

      "rootfs/ubuntu-20-04-x86-64/debug-info" = rootfsPkgs."rootfs/ubuntu-20-04-x86-64/debug-info";
      "rootfs/ubuntu-22-04-x86-64/debug-info" = rootfsPkgs."rootfs/ubuntu-22-04-x86-64/debug-info";
      "rootfs/ubuntu-24-04-x86-64/debug-info" = rootfsPkgs."rootfs/ubuntu-24-04-x86-64/debug-info";
      "rootfs/ubuntu-24-04-i386/debug-info" = rootfsPkgs."rootfs/ubuntu-24-04-i386/debug-info";
      "rootfs/ubuntu-24-04-arm/debug-info" = rootfsPkgs."rootfs/ubuntu-24-04-arm/debug-info";
      "rootfs/ubuntu-24-04-aarch64/debug-info" = rootfsPkgs."rootfs/ubuntu-24-04-aarch64/debug-info";
      "rootfs/ubuntu-24-04-s390x/debug-info" = rootfsPkgs."rootfs/ubuntu-24-04-s390x/debug-info";
      "rootfs/debian-bookworm-mipsel/debug-info" = rootfsPkgs."rootfs/debian-bookworm-mipsel/debug-info";
      "rootfs/debian-buster-mips/debug-info" = rootfsPkgs."rootfs/debian-buster-mips/debug-info";

      "rootfs/ubuntu-20-04-x86-64/models" = rootfsPkgs."rootfs/ubuntu-20-04-x86-64/models";
      "rootfs/ubuntu-22-04-x86-64/models" = rootfsPkgs."rootfs/ubuntu-22-04-x86-64/models";
      "rootfs/ubuntu-24-04-x86-64/models" = rootfsPkgs."rootfs/ubuntu-24-04-x86-64/models";
      "rootfs/ubuntu-24-04-i386/models" = rootfsPkgs."rootfs/ubuntu-24-04-i386/models";
      "rootfs/ubuntu-24-04-arm/models" = rootfsPkgs."rootfs/ubuntu-24-04-arm/models";
      "rootfs/ubuntu-24-04-aarch64/models" = rootfsPkgs."rootfs/ubuntu-24-04-aarch64/models";
      "rootfs/ubuntu-24-04-s390x/models" = rootfsPkgs."rootfs/ubuntu-24-04-s390x/models";
      "rootfs/debian-bookworm-mipsel/models" = rootfsPkgs."rootfs/debian-bookworm-mipsel/models";
      "rootfs/debian-buster-mips/models" = rootfsPkgs."rootfs/debian-buster-mips/models";

      inherit (win32metadataPkgs) win32metadata;
      "win32metadata/pdbs/x86-64" = win32metadataPkgs."win32metadata/pdbs/x86-64";
      "win32metadata/pdbs/i386" = win32metadataPkgs."win32metadata/pdbs/i386";
      "win32metadata/pdbs/aarch64" = win32metadataPkgs."win32metadata/pdbs/aarch64";
      "win32metadata/pdbs/x86-64/models" = win32metadataPkgs."win32metadata/pdbs/x86-64/models";
      "win32metadata/pdbs/i386/models" = win32metadataPkgs."win32metadata/pdbs/i386/models";
      "win32metadata/pdbs/aarch64/models" = win32metadataPkgs."win32metadata/pdbs/aarch64/models";
    }
  );
in
removeAttrs scope [
  "callPackage"
  "newScope"
  "overrideScope"
  "overrideScope'"
  "packages"
  "pkgs"
  "pkgs-2505"
  "stdenv"
  "ccacheStdenv"
  "lib"
  "system"
  "inputs"
  "msvc"
  "mkModels"
  "crossToolchains"
  "revngPackages"
]
