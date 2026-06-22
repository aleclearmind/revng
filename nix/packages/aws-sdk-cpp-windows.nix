{ pkgs, inputs, windowsCross }:
# Cross-build of aws-sdk-cpp (S3 API only) targeting x86_64-w64-mingw32.
# Matches the host `aws-sdk-cpp.nix` configuration (apis=["s3"], FORCE_CURL,
# ENABLE_RTTI=OFF, CPP_STANDARD=20).
#
# The dependency chain (aws-c-* family + aws-crt-cpp + aws-sdk-cpp) has
# two cross-build obstacles in nixpkgs:
#
# 1. `meta.platforms = lib.platforms.unix` on every aws-* package. AWS
#    upstream supports Windows; nixpkgs just hasn't declared it. Widen
#    each one to include `lib.platforms.windows`.
#
# 2. Every aws-c-* package lists `s2n-tls` as a buildInput. s2n-tls is
#    POSIX-only (the upstream README explicitly excludes Windows), and
#    on Windows aws-c-io's CMakeLists detects the platform and uses
#    SChannel instead. Filter s2n-tls out of every aws-c-*'s
#    buildInputs so the closure doesn't try to cross-build it.
#
# The C parts (aws-c-*) use the default cross-mingw stdenv (gcc-mingw)
# — they're pure C with stable ABI across toolchains. The C++ parts
# (aws-crt-cpp, aws-sdk-cpp) use the same libcxxStdenv overlay as
# `windows/llvm` / `windows/qemu` / `windows/boost` for consistent
# libc++ linkage with revng itself.
let
  lib = pkgs.lib;
  # Extend the shared `windowsCross` (libcxxStdenv overlay — see
  # `windows-cross.nix`) with the aws-specific overrides: widen
  # meta.platforms, drop s2n-tls buildInput, fix http3-via-curl.
  windowsCrossAws = windowsCross.extend (
    _final: prev:
    let
      widenPlat = pkg: pkg.overrideAttrs (old: {
        meta = (old.meta or { }) // {
          platforms = (old.meta.platforms or [ ]) ++ lib.platforms.windows;
        };
      });
      dropS2n = pkg: pkg.overrideAttrs (old: {
        buildInputs = builtins.filter (
          d: !(lib.hasPrefix "s2n-tls" (d.name or ""))
        ) (old.buildInputs or [ ]);
      });
      fixupAwsC = pkg: widenPlat (dropS2n pkg);
    in
    {
      # curl is needed by aws-sdk-cpp under FORCE_CURL=ON. Disable
      # http3 — nghttp3 is missing from cross-mingw's platform list
      # and S3 doesn't need it anyway.
      curl = prev.curl.override { http3Support = false; };

      aws-c-common = fixupAwsC prev.aws-c-common;
      aws-c-cal = fixupAwsC prev.aws-c-cal;
      aws-c-compression = fixupAwsC prev.aws-c-compression;
      aws-c-io = fixupAwsC prev.aws-c-io;
      aws-c-http = fixupAwsC prev.aws-c-http;
      aws-c-auth = fixupAwsC prev.aws-c-auth;
      aws-c-mqtt = fixupAwsC prev.aws-c-mqtt;
      aws-c-s3 = fixupAwsC prev.aws-c-s3;
      aws-c-event-stream = fixupAwsC prev.aws-c-event-stream;
      aws-c-sdkutils = fixupAwsC prev.aws-c-sdkutils;
      aws-checksums = fixupAwsC prev.aws-checksums;
    }
  );
  windowsStdenv = windowsCrossAws.llvmPackages_21.libcxxStdenv;

  aws-crt-cpp = (windowsCrossAws.callPackage
    "${inputs.nixpkgs}/pkgs/by-name/aw/aws-crt-cpp/package.nix"
    {
      stdenv = windowsStdenv;
    }
  ).overrideAttrs (old: {
    # Same s2n-tls drop as the C-level aws-c-* packages — aws-crt-cpp
    # propagates s2n-tls, but on Windows it's unused (SChannel-only).
    propagatedBuildInputs = builtins.filter (
      d: !(lib.hasPrefix "s2n-tls" (d.name or ""))
    ) (old.propagatedBuildInputs or [ ]);
    # libcxxStdenv consumers need libunwind on the link line to resolve
    # `_Unwind_Resume` — same plumbing as `windows/llvm` uses for the
    # cross-mingw libunwind. (qemu's libtcg doesn't use C++ exceptions,
    # so it skipped this; aws-crt-cpp does.)
    NIX_LDFLAGS_x86_64_w64_mingw32 =
      "-L${windowsCrossAws.llvmPackages_21.libunwind}/lib -lunwind";
    meta = (old.meta or { }) // {
      platforms = (old.meta.platforms or [ ]) ++ lib.platforms.windows;
    };
  });

  aws-sdk-cpp = (windowsCrossAws.callPackage
    "${inputs.nixpkgs}/pkgs/by-name/aw/aws-sdk-cpp/package.nix"
    {
      stdenv = windowsStdenv;
      inherit aws-crt-cpp;
      apis = [ "s3" ];
    }
  ).overrideAttrs (oldAttrs: {
    cmakeFlags = oldAttrs.cmakeFlags ++ [
      "-DENABLE_TESTING=OFF"
      "-DFORCE_CURL=ON"
      "-DENABLE_UNITY_BUILD=OFF"
      "-DENABLE_RTTI=OFF"
      "-DCPP_STANDARD=20"
    ];
    NIX_LDFLAGS_x86_64_w64_mingw32 =
      "-L${windowsCrossAws.llvmPackages_21.libunwind}/lib -lunwind";
    # AWS SDK uses MSVC-style capitalised SDK headers (WinSock2.h,
    # Userenv.h, …). mingw-w64 only ships the lowercase variants and
    # we're cross-compiling on a case-sensitive filesystem, so the
    # uppercase includes fail to resolve. Lowercase them in-tree.
    postPatch = (oldAttrs.postPatch or "") + ''
      find src/aws-cpp-sdk-core -type f \( -name '*.cpp' -o -name '*.h' \) -exec sed -i \
        -e 's|<Userenv\.h>|<userenv.h>|g' \
        -e 's|<Windows\.h>|<windows.h>|g' \
        -e 's|<WinInet\.h>|<wininet.h>|g' \
        -e 's|<WinSock2\.h>|<winsock2.h>|g' \
        -e 's|<Ws2ipdef\.h>|<ws2ipdef.h>|g' \
        -e 's|<Ws2tcpip\.h>|<ws2tcpip.h>|g' \
        {} +
      # cmake/platform/windows.cmake names the system lib as `Userenv`
      # (capitalised), which mingw-w64 ships only as `libuserenv.a`.
      substituteInPlace cmake/platform/windows.cmake \
        --replace-fail "Userenv version ws2_32" "userenv version ws2_32"
    '';
    meta = (oldAttrs.meta or { }) // {
      platforms = (oldAttrs.meta.platforms or [ ]) ++ lib.platforms.windows;
    };
  });
in
aws-sdk-cpp
