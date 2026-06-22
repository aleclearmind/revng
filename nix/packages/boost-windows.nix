{ pkgs, inputs, windowsCross }:
# Cross-build of boost 1.81 targeting x86_64-w64-mingw32 — pinned to
# the same version as the Linux `boost.nix` so the two ABIs stay in
# lockstep across revng's host/target boundary.
#
# Uses the shared `windowsCross` (libcxxStdenv overlay — see
# `windows-cross.nix`) so boost links against the COFF-export-fixed
# libc++.dll.
(pkgs.lib.fix (
  self:
  windowsCross.callPackage "${inputs.nixpkgs}/pkgs/development/libraries/boost/1.81.nix" {
    stdenv = windowsCross.llvmPackages_21.libcxxStdenv;
    # boost-build runs on the build host (b2 + bootstrap), so pull it
    # from the host (linux) pkgs rather than the cross set. Same
    # `useBoost = self` trick as `boost.nix` so b2's version matches
    # our pinned 1.81 source.
    boost-build = pkgs.boost-build.override { useBoost = self; };
  }
)).overrideAttrs
  (oldAttrs: {
    # Match the Linux boost's lib subset — revng only uses Boost.Test.
    configureFlags = oldAttrs.configureFlags ++ [ "--with-libraries=test" ];
    # boost/generic.nix's postFixup does `$RANLIB "$out/lib/"*.a` —
    # with `--with-libraries=test` (and the default shared build on
    # mingw) the glob matches nothing, ranlib errors with "missing
    # operand", and nixpkgs' `set -e` aborts the build. Make the call
    # tolerant.
    postFixup = pkgs.lib.replaceStrings
      [ ''$RANLIB "$out/lib/"*.a'' ]
      [ ''for f in "$out/lib/"*.a; do [ -e "$f" ] && $RANLIB "$f"; done || true'' ]
      (oldAttrs.postFixup or "");
  })
