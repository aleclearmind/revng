{ nixpkgs, system }:
let
  pkgs = import nixpkgs { inherit system; };

  # Build a thin derivation whose bin/ exposes every `${from}-X` binary
  # from `cc` as `${to}-X`. Used because nixpkgs only accepts a small
  # set of vendors (pc/unknown/...), but revng-qa expects gentoo/ibm/
  # hardfloat-flavored triples.
  tripleAlias =
    { from, to, cc }:
    pkgs.runCommand "${to}-cc-alias" { } ''
      mkdir -p $out/bin
      for f in ${cc}/bin/${from}-*; do
        ln -s "$f" "$out/bin/${to}-''${f##*/${from}-}"
      done
    '';

  crossCompiler = (
    triple: binutilsVersion: binutilsHash: gccVersion:
    let
      crossNixpkgs = import nixpkgs {
        crossSystem = {
          config = triple;
        };
        inherit system;
      };
      pkgs2 = crossNixpkgs.buildPackages;
      binutils_old_real = pkgs2.bintools.bintools.overrideAttrs {
        version = binutilsVersion;
        patches = [ ];
        outputs = [
          "out"
          "info"
          "man"
          "dev"
        ]; # Omit lib

        src = pkgs.fetchurl {
          url = "mirror://gnu/binutils/binutils-${binutilsVersion}.tar.bz2";
          hash = binutilsHash;
        };
      };
      binutils_old = pkgs2.wrapBintoolsWith {
        bintools = binutils_old_real;
      };
      pkgs3 = pkgs2 // {
        #binutils = binutils_old;
        #bintools = binutils_old;

        # NOTE: we might want to use binutils_old for stdenvNoLibc as well, whih
        # is currently otherwise built with non-custom versions of GCC/binutils
        stdenv = pkgs2.stdenv // {
          cc = pkgs2.stdenv.cc // {
            bintools = binutils_old;
          };
        };

        # TODO: we're not applying uclibc configuration. The following does not work:
        # stdenvNoLibc = pkgs2.stdenvNoLibc // {
        #   hostPlatform = pkgs2.hostPlatform // {
        #     uclibc = pkgs2.hostPlatform.uclibc // {
        #       extraConfig = "${builtins.readFile ./uClibc.config}";
        #     };
        #   };
        # };
      };
    in
    (pkgs3.wrapCCWith {
      cc = (
        pkgs3.callPackage (nixpkgs + "/pkgs/development/compilers/gcc/default.nix") (
          {
            majorMinorVersion = gccVersion;
            noSysDirs = true;
            #binutils = binutils_old;
            targetPackages = pkgs3;
            libcCross = pkgs3.targetPackages.libc;
          }
          // (
            if pkgs.lib.hasInfix "mingw" triple then
              {
                threadsCross = {
                  model = "win32";
                  package = null;
                };
              }
            else
              { }
          )
        )
      );
      bintools = binutils_old;
    })
  );
in
[
  # crossNixpkgs.buildPackages.gcc9
  # (crossNixpkgs.buildPackages.gcc9.override {
  #   bintools = crossNixpkgs.buildPackages.gcc9.bintools.overrideAttrs {
  #     version = "2.35";
  #     bintools = crossNixpkgs.buildPackages.gcc9.bintools.bintools.overrideAttrs {
  #       version = "2.35";
  #       patches = [];
  #       outputs = [
  #         "out"
  #         "info"
  #         "man"
  #         "dev"
  #       ]; # Omit lib

  #       src = pkgs.fetchurl {
  #         url = "mirror://gnu/binutils/binutils-2.35.tar.bz2";
  #         hash = "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c=";
  #       };
  #     };
  #   };
  # })

  # (
  #   crossNixpkgs.buildPackages.pkgs.wrapCC (
  #     crossNixpkgs.buildPackages.pkgs.callPackage (<nixpkgs> + "/pkgs/development/compilers/gcc/default.nix") {
  #       majorMinorVersion = "9";
  #       noSysDirs = true;
  #     }
  #   )
  # )

  # TODO: pin musl version, we wanted 1.1.12, we're getting 1.2.5
  # TODO: pin uclibc version, we wanted ???, we're getting ???

  # We wanted GCC 9.2.0 (got 9.5.0)
  (crossCompiler "mips-unknown-linux-musl" "2.35"
    "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c="
    "13"
  )
  (crossCompiler "mipsel-unknown-linux-musl" "2.35"
    "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c="
    "13"
  )
  # We wanted GCC 7.3.0 (got 9.5.0)
  (crossCompiler "aarch64-unknown-linux-musl" "2.35"
    "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c="
    "13"
  )
  # Build the cross-compilers with vendors nixpkgs knows (pc/unknown);
  # the orchestra-flavored vendor names (gentoo/ibm/hardfloat) come in
  # as symlinks via `tripleAlias` below.
  (crossCompiler "armv7a-unknown-linux-uclibceabihf" "2.35"
    "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c="
    "13"
  )
  (crossCompiler "x86_64-unknown-linux-musl" "2.35"
    "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c="
    "13"
  )
  (crossCompiler "i686-unknown-linux-musl" "2.35"
    "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c="
    "13"
  )
  (crossCompiler "s390x-unknown-linux-musl" "2.35"
    "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c="
    "13"
  )
  # mingw cross-compilers (unchanged).
  (crossCompiler "i686-w64-mingw32" "2.35" "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c=" "13")
  (crossCompiler "x86_64-w64-mingw32" "2.35" "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c="
    "13"
  )

  # The bumped revng-qa expects gentoo/ibm/hardfloat-vendor triple
  # names. nixpkgs doesn't recognize those vendors, so expose them as
  # symlinks pointing at the binaries above.
  (tripleAlias {
    from = "armv7a-unknown-linux-uclibceabihf";
    to = "armv7a-hardfloat-linux-uclibceabi";
    cc = (crossCompiler "armv7a-unknown-linux-uclibceabihf" "2.35"
      "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c=" "13");
  })
  (tripleAlias {
    from = "x86_64-unknown-linux-musl";
    to = "x86_64-gentoo-linux-musl";
    cc = (crossCompiler "x86_64-unknown-linux-musl" "2.35"
      "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c=" "13");
  })
  (tripleAlias {
    from = "i686-unknown-linux-musl";
    to = "i386-gentoo-linux-musl";
    cc = (crossCompiler "i686-unknown-linux-musl" "2.35"
      "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c=" "13");
  })
  (tripleAlias {
    from = "s390x-unknown-linux-musl";
    to = "s390x-ibm-linux-musl";
    cc = (crossCompiler "s390x-unknown-linux-musl" "2.35"
      "sha256-fSRmD4cJNnBzjli8x7ewbxIcD8sMqPxENo1nWl75z/c=" "13");
  })
]
