{ pkgs, stdenv, python,
  revngPythonDependencies, aws-sdk-cpp, boost,
  revngJavascriptDependencies, llvm, qemu, qemuHelpers, nanobind,
  revngClang, revngPackages,
}:
let
  # System-wide revng configuration consumed via -DREVNG_SYSTEM_CONFIG.
  # Compared with the orchestra-shipped revng.yml (3 entries:
  # --sysroot=$ROOT/link-only, -rpath=$ROOT/lib, -L$ROOT/lib), the
  # first six -L lines below are just the orchestra `--sysroot`
  # exploded across the separate nix store paths each library lives
  # in. The last two are genuinely new:
  #   --copy-dt-needed-entries:
  #     ld.bfd defaults to --no-copy-dt-needed-entries on nixpkgs;
  #     LinkForTranslation passes `-lgcc` (static), and that static
  #     libgcc references _Unwind_RaiseException from libgcc_s, so
  #     the linker has to chase implicit DSO deps.
  #   -lglib-2.0:
  #     qemu's syscall.c references g_memdup; orchestra got it from
  #     the consolidated --sysroot, here we name it explicitly.
  revngSystemConfig = pkgs.writeText "revng.yml" ''
    translation-ldflags:
    - -L${pkgs.glibc}/lib
    - -L${pkgs.gcc-unwrapped}/lib/gcc/x86_64-unknown-linux-gnu/${pkgs.gcc-unwrapped.version}
    - -L${pkgs.gcc-unwrapped.lib}/lib
    - -L${pkgs.libunwind}/lib
    - -L${pkgs.glib.out}/lib
    - -L${pkgs.zlib}/lib
    - --copy-dt-needed-entries
    - -lglib-2.0
  '';
in
stdenv.mkDerivation {
  name = "revng";

  # Filter the source so unrelated repo-level files (flake.nix,
  # result symlink, dev junk) don't re-hash revng on every edit.
  src = pkgs.lib.cleanSourceWith {
    src = ../..;
    filter =
      path: type:
      let
        base = baseNameOf path;
      in
      !(
        base == "flake.nix"
        || base == "flake.lock"
        || base == "nix"
        || base == "result"
        || base == "TODO"
        || pkgs.lib.hasSuffix ".iso" base
        || pkgs.lib.hasSuffix ".iso.1" base
        || base == ".claude"
      );
  };

  nativeBuildInputs = with pkgs; [
    revngPythonDependencies
    clang-tools
    aws-sdk-cpp
    boost
    cmake
    codespell
    doxygen
    git
    jq
    libarchive
    ninja
    nodejs
    sqlite
    unzip
    zstd
    revngJavascriptDependencies
    llvm
    qemu
    nanobind
    zlib
  ];

  # The repository has ~15 build/test scripts shebanged with
  # `#!/usr/bin/env <bash|python3>`; rewrite them at build time so
  # they don't depend on `/usr/bin/env` (absent inside the sandbox).
  postPatch = ''
    patchShebangs --build .
  '';

  cmakeFlags = [
    "-GNinja"
    "-DCMAKE_CXX_STANDARD=20"
    "-DCMAKE_C_FLAGS=-O2"
    "-DCMAKE_CXX_FLAGS=-O2"
    "-DCMAKE_BUILD_TYPE=Debug"
    "-DLLVM_DIR=${llvm}/lib/cmake/llvm"
    "-DLIBTCG_DIR=${qemu}"
    "-DQEMU_HELPERS_DIR=${qemuHelpers}"
    "-DTEST_REVNG_QA_DIR=${revngPackages."test/revng-qa"}"
    "-DTARGET_CLANG=${revngClang}/bin/clang"
    "-DREVNG_SYSTEM_CONFIG=${revngSystemConfig}"
  ];

  doCheck = true;

  checkPhase = ''
    # mlir-lit-tests invoke FileCheck. Use nixpkgs' llvm_21 — it
    # already comes via test/revng-qa's nativeBuildInputs (so no
    # extra closure cost) and ships FileCheck in bin/ directly,
    # avoiding our patched llvm's CMAKE_INSTALL_BINDIR=libexec quirk.
    export PATH="${pkgs.llvm_21}/bin:$PATH"
    # ctest spawns processes (pip during install-all-wheels, pytest's
    # cache, …) that touch $HOME; the sandbox's /homeless-shelter is
    # not writable.
    export HOME="$TMPDIR/home"
    mkdir -p "$HOME"
    ctest -j$(nproc)
  '';

}
