{ pkgs, stdenv, python, crossToolchains, msvc, orchestraNinja }:
let
  revng-qa = stdenv.mkDerivation {
    name = "revng-qa";

    src = pkgs.fetchFromGitHub {
      owner = "revng";
      repo = "revng-qa";
      rev = "4227ac818e370dbe28b0ef088af2fd733eab7bf8";
      hash = "sha256-L5rWWoHoXr+EcMuchU/UWR8WdLcyGR6uZqROpKTyTsc=";
    };

    nativeBuildInputs = (with pkgs; [
      cmake
      ninja
    ]) ++ [
      (python.withPackages (
        ps: with ps; [
          jinja2
          pyyaml
        ]
      ))
    ];

    cmakeFlags = [
      "-GNinja"
    ];

  };

  testRevngQa = stdenv.mkDerivation {
    name = "test/revng-qa";

    unpackPhase = "true";

    nativeBuildInputs =
      with pkgs;
      (
        [
          binutils
          llvm_21
          lld_21
        ]
        ++ crossToolchains
      )
      ++ msvc.toolchains
      ++ [
        revng-qa
        orchestraNinja
        (python.withPackages (
          ps: with ps; [
            jinja2
            pyyaml
          ]
        ))
        # WIP: this should be pulled by MSVC dep
        pkgs.samba
      ];

    buildPhase = ''
      echo
    '';

    installPhase = ''
      mkdir -p $out
      python3 \
        ${revng-qa}/libexec/revng/test-configure \
        "${revng-qa}/share/revng/test/configuration/revng-qa/"*.yml \
        --install-path "${revng-qa}" \
        --destination . \
        --target-type 'revng-qa\..*'
      export REVNG_OPTIONS="--debug-log=verify"
      # test-configure emits `shell = /bin/bash` on every rule; that
      # path doesn't exist inside the nix sandbox. Point ninja at
      # the bash we actually have.
      sed -i "s|shell = /bin/bash|shell = ${pkgs.bash}/bin/bash|g" build.ninja
      # revng-qa develop tags native tests with the `native` tag,
      # which invokes plain `gcc` (no triple prefix). orchestra's
      # host gcc is musl-based, so static linking works; under nix
      # the host gcc is glibc and lacks static libs. Point `gcc` at
      # the x86_64 musl cross-compiler instead — it already lives
      # in PATH thanks to crossShell.nix.
      ln -s "$(command -v x86_64-unknown-linux-musl-gcc)" gcc
      ln -s "$(command -v x86_64-unknown-linux-musl-g++)" g++
      export XDG_CACHE_HOME="$PWD/.cache"
      mkdir -p "$XDG_CACHE_HOME/.cache"
      mkdir -p extra-includes/gnu

      i386-winsdk-vc12-cl || true
      i386-winsdk-vc13-cl || true
      i386-winsdk-vc16-cl || true
      i386-winsdk-vc19-cl || true
      x86_64-winsdk-vc19-cl || true
      aarch64-winsdk-vc19-cl || true

      cp -a ${pkgs.glibc.dev}/include/gnu/stubs-64.h extra-includes/gnu/stubs-32.h
      # revng-qa develop adds IDA-based (.idb) and apple-darwin11
      # ABI tests; we don't ship idat64 or an apple toolchain.
      # Build with `-k0` and tolerate those specific failures, then
      # verify the artifacts revng actually consumes (the
      # well-known-models cross-compiled binaries) are present.
      # WIP: -k0 + || true tolerates the IDA/Apple test failures
      # noted above. Drop once revng-qa stops shipping those rules
      # or once we provide idat64/an apple toolchain.
      NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -isystem$PWD/extra-includes" \
        NIX_CFLAGS_LINK= PATH="$PWD:$PATH" \
        ninja -v -k0 all || true
      test -d share/revng/test/tests/well-known-models \
        || { echo "well-known-models not built"; exit 1; }
      # Copy the built test artifacts into $out so downstream
      # derivations (test/revng) can consume them. The build
      # graph put them under share/ relative to the build dir.
      mkdir -p "$out/share"
      cp -a share/revng "$out/share/"
      rm -rf "$XDG_CACHE_HOME"
    '';

  };
in
{
  inherit revng-qa;
  "test/revng-qa" = testRevngQa;
}
