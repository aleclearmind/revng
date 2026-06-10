{ pkgs, stdenv, python, crossToolchains, msvc, ninjaShellRule, inputs, revngPackages }:
let
  revng-qa = stdenv.mkDerivation {
    name = "revng-qa";

    # WIP: use the local revng-qa tree (via the `revng-qa` flake
    # input pointing at path:/home/nix/revng-qa) so we can iterate
    # on the test-configuration *.yml files. Switch back to a
    # github: URL once the local changes are upstreamed.
    src = inputs.revng-qa;

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

    # nixpkgs's gcc-wrapper defaults to enabling `zerocallusedregs`,
    # which adds `-fzero-call-used-regs=used-gpr`. That makes the
    # compiler emit `xor %edx,%edx; xor %ecx,%ecx; …` before every
    # `ret` (clearing non-callee-saved GPRs as a Spectre-style
    # gadget hardening). revng's DetectABI then sees those zeroed
    # registers as "written and never read at exit" and infers them
    # as part of the return-value register set — c-operator-precedence
    # decompiles to `struct_82 { offset_0; offset_8 }` instead of a
    # plain `uint64_t`. Test binaries are inputs to the analyzer; we
    # need their codegen to match what orchestra produces, which
    # means no hardening fixups.
    hardeningDisable = [ "zerocallusedregs" ];

    nativeBuildInputs =
      with pkgs;
      (
        [
          binutils
          gcc
          llvm_21
          lld_21
        ]
        ++ crossToolchains
      )
      ++ msvc.toolchains
      ++ [
        revngPackages."macos/clang/x86-64"
        revngPackages."macos/clang/i686"
        revngPackages."macos/clang/arm"
        revngPackages."macos/clang/aarch64"
      ]
      ++ [
        revng-qa
        ninjaShellRule
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
      # stdenv's mold-linker setup exports NIX_CFLAGS_LINK / NIX_LDFLAGS
      # that the cross-toolchains pick up and then fail to resolve at
      # link time. Clear them so each cross-gcc finds its own ld.
      NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -isystem$PWD/extra-includes" \
        NIX_CFLAGS_LINK= \
        ninja all
      # Copy the built test artifacts into $out so downstream
      # derivations (test/revng) can consume them. The build
      # graph put them under share/ relative to the build dir.
      mkdir -p "$out/share"
      cp -a share/revng "$out/share/"
    '';

  };
in
{
  inherit revng-qa;
  "test/revng-qa" = testRevngQa;
}
