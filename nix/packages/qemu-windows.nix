{ pkgs, python, inputs, windowsCross }:
# Cross-build of revng's QEMU fork targeting x86_64-w64-mingw32.
# Only the `libtcg` targets (the part revng actually consumes via
# `LIBTCG_DIR`) — no `linux-user` (Linux-specific syscall emulation,
# meaningless on Windows) and no `llvm-helpers` variant (that's the
# separate qemu-helpers derivation we're not porting yet).
#
# Extends the shared `windowsCross` (libcxxStdenv overlay — see
# `windows-cross.nix`) with the qemu-specific glib override.
let
  windowsCrossQemu = windowsCross.extend (
    final: prev: {
      # `glib` in nixpkgs cross-mingw has two issues we have to fix:
      #
      # (1) It transitively pulls in the cross-mingw `python3` (via
      #     `python3Packages.{packaging,wrapPython}` in its
      #     nativeBuildInputs), and cross-mingw python3 is marked
      #     broken/unsupported on windows. Python is only needed at
      #     build time (codegen scripts, version headers, …), so swap
      #     it for the HOST python via buildPackages.
      #
      # (2) It tries to link `libsysprof-capture` unconditionally
      #     (except on FreeBSD). That library uses POSIX-only headers
      #     like <endian.h> / <poll.h>, so it fails to cross-compile
      #     for mingw. Disable it via meson flag and drop the
      #     buildInput.
      glib = (prev.glib.override {
        python3 = final.buildPackages.python3;
        python3Packages = final.buildPackages.python3Packages;
        libsysprof-capture = null;
      }).overrideAttrs (old: {
        mesonFlags = (old.mesonFlags or [ ]) ++ [ "-Dsysprof=disabled" ];
        buildInputs = builtins.filter (
          d: !(prev.lib.hasPrefix "libsysprof-capture" (d.name or ""))
        ) (old.buildInputs or [ ]);
      });
    }
  );
  windowsStdenv = windowsCrossQemu.llvmPackages_21.libcxxStdenv;
in
windowsStdenv.mkDerivation {
  name = "windows-qemu";

  # WIP: local revng-qemu tree on `feature/windows` (see flake.nix).
  # The Windows-specific source patches live there as proper commits
  # (target/meson.build hexagon gate, libtcg meson rework, syscall_nr
  # stubs, osdep sigset_t typedef, cpu_abort sigaction skip, …).
  # Switch back to fetchFromGitHub on the upstreamed branch when ready.
  src = inputs.qemu;

  postPatch = ''
    patchShebangs python/scripts/link-embedded-objects

    # Same source tweaks as the host qemu build (drop fp tests, drop
    # _Static_assert in target/i386/cpu.h, drop ASSERT_CONSTANT in
    # libtcg.c). These are not Windows-specific so they live in the
    # nix postPatch rather than the qemu fork's history.
    grep -vF "subdir('fp')" tests/meson.build > tests/meson.build2
    mv tests/meson.build2 tests/meson.build
    grep -vF "_Static_assert" target/i386/cpu.h > target/i386/cpu.h2
    mv target/i386/cpu.h2 target/i386/cpu.h
    grep -vF "ASSERT_CONSTANT" libtcg/libtcg.c > libtcg/libtcg.c2
    mv libtcg/libtcg.c2 libtcg/libtcg.c
  '';

  preBuild = ''
    # windres (used by meson for version.rc) hard-codes
    # `x86_64-w64-mingw32-gcc` as its preprocessor and we only have
    # the cross-mingw clang wrapper. Shim the gcc name to clang in a
    # tmp dir at the front of PATH so windres finds something.
    GCCSHIM=$PWD/.gccshim
    mkdir -p "$GCCSHIM"
    ln -sf "$(command -v x86_64-w64-mingw32-clang)" "$GCCSHIM/x86_64-w64-mingw32-gcc"
    export PATH="$GCCSHIM:$PATH"

    cd build
  '';

  # cross-glib propagates `gettext-mingw` and `libiconv-mingw`, whose
  # setup-hooks `export NIX_LDFLAGS+=" -lintl -liconv"` at role 0
  # (host of our cross stdenv = mingw). The un-salted `NIX_LDFLAGS`
  # is also read by the BUILD-machine clang (host linux), where
  # `-lintl` / `-liconv` don't exist (libintl is part of glibc on
  # Linux). Meson's native compiler sanity check then fails. Clear
  # the un-salted vars before configure — the cross-target salted
  # `NIX_LDFLAGS_x86_64_w64_mingw32` is preserved.
  preConfigure = ''
    unset NIX_LDFLAGS
    unset NIX_CFLAGS_COMPILE
  '';

  # All build-side tooling stays on the host (meson, ninja, python,
  # pkg-config) — the cross-stdenv handles the actual compile/link via
  # $CC = `x86_64-w64-mingw32-clang`. We also need a *build-machine*
  # C/C++ compiler for meson's cross-build mode (qemu's meson.build
  # iterates `all_languages` including the native build variants for
  # code generators), hence `llvmPackages_21.clang` here.
  nativeBuildInputs = (with pkgs; [
    pkg-config
    meson
    ninja
    coreutils-full
    llvmPackages_21.clang
  ]) ++ [
    (python.withPackages (python-pkgs: [ python-pkgs.distlib ]))
    python.pkgs.python
  ];

  # Cross-mingw buildInputs. glib is qemu's main runtime dep (used
  # even by libtcg-only builds for hash tables, threading, etc.).
  # Our overlay above swaps glib's build-time python from the broken
  # cross-mingw one to the host python.
  buildInputs = [
    windowsCrossQemu.zlib
    windowsCrossQemu.glib
    windowsCrossQemu.windows.pthreads
  ];

  dontUseMesonConfigure = true;
  enableParallelBuilding = true;

  # Collect as many compile errors as possible per iteration
  # (`ninja -k 0` keeps going past failures) — saves round-trips
  # while we're chasing porting issues.
  ninjaFlags = [ "-k" "0" ];

  configureFlags =
    let
      targets = builtins.concatStringsSep "," (map (architecture: "${architecture}-libtcg") [
        "arm"
        "aarch64"
        "i386"
        "mips"
        "mipsel"
        "s390x"
        "x86_64"
      ]);
    in
    [
      # Tell qemu's `./configure` to use the cross-mingw clang and
      # the matching binutils tools (ar, ranlib, strip, …).
      "--cc=x86_64-w64-mingw32-clang"
      "--cxx=x86_64-w64-mingw32-clang++"
      "--cross-prefix=x86_64-w64-mingw32-"
      # Build-machine compiler for native code generators (hexagon's
      # `gen_semantics.c` is built with `native: true` and needs a
      # linux compiler regardless of the cross target). Qemu's
      # configure calls this `--host-cc` (gnu autoconf-style naming
      # where "host" = where the build runs). The cross stdenv only
      # puts mingw `cc` on PATH, so pass the absolute path to host
      # clang.
      "--host-cc=${pkgs.llvmPackages_21.clang}/bin/clang"
      "--target-list=${targets}"
      # nixpkgs' cmake/meson setup-hook would inject --prefix=$out
      # automatically, but qemu's own configure (dontUseMesonConfigure)
      # bypasses that. Without it, meson install errors with
      # 'PermissionError: /qemu' (default prefix /usr/local/qemu).
      "--prefix=${placeholder "out"}"

      "--disable-plugins"
      "--disable-werror"
      "--disable-docs"
      "--disable-kvm"
      "--disable-tools"
      "--disable-system"
      "--disable-libnfs"
      "--disable-vde"
      "--disable-gnutls"
      "--disable-cap-ng"
      "--disable-pie"
      # `linux-user` mode is what we'd disable on Windows but we're
      # already only asking for libtcg targets, so it's not built.
      "-Dvhost_user=disabled"
      "-Dxkbcommon=disabled"

      "--extra-cflags=-Wno-unused-variable"
      "--extra-cflags=-Wno-unused-function"
      "--extra-cflags=-Wno-unused-result"
      "--extra-cflags=-Wno-unused-but-set-variable"
      # Several qemu sources call POSIX `bzero` etc. without
      # including `<strings.h>`. mingw clang errors on implicit
      # function declarations under C99+ — demote to a warning so
      # we don't chase each missing include.
      "--extra-cflags=-Wno-error=implicit-function-declaration"
      "--extra-cflags=-Wno-implicit-function-declaration"
      # Qemu's link line includes the `-pthread` flag, which clang's
      # mingw driver translates to `-lpthread`. mingw-w64 provides
      # the same primitives via `libwinpthread.dll.a` *and* a
      # `libpthread.dll.a` alias — but our buildInputs setup-hooks
      # only populate `NIX_LDFLAGS_<salt>`, which qemu's meson-built
      # link commands don't honour (no `-L` paths on the final cmd).
      # Pass the pthreads lib dir explicitly via --extra-ldflags.
      "--extra-ldflags=-L${windowsCrossQemu.windows.pthreads}/lib"
    ];

  preInstall = ''
    mkdir -p $out/include
    mkdir -p $out/lib
  '';

  # Wrap configurePhase so if qemu's meson setup fails, the actual
  # meson-log.txt (which has the compile-test stderr) is printed
  # rather than just the generic meson.build:216 message.
  configurePhase = ''
    runHook preConfigure
    if ! ./configure $configureFlags; then
      echo "=========== meson-log.txt ==========="
      cat build/meson-logs/meson-log.txt || true
      echo "=========== end meson-log.txt ==========="
      exit 1
    fi
    runHook postConfigure
  '';
}
