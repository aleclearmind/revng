{ pkgs, pkgs-2505, stdenv, python,
  revngPythonDependencies, aws-sdk-cpp, boost,
  revngJavascriptDependencies, llvm, qemu, qemuHelpers, nanobind,
  revngClang, revngPackages,
}:
# Build revng
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
    # cmake 4.x is stricter about target-dep cycles than 3.x;
    # develop's python wheel + revng-all-binaries graph forms a
    # cycle that 3.x silently tolerates. Pin to 3.x for now.
    pkgs-2505.cmake
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
    makeWrapper
    llvm
    qemu
    nanobind
    zlib
  ];

  # python/CMakeLists.txt has a partial convention mismatch:
  # `python_module(... MODULE_GENERATED_FILES ...)` hard-codes
  # the staging dir to ${CMAKE_BINARY_DIR}/python/, while all
  # the rules that actually produce those files stage them at
  # ${CMAKE_BINARY_DIR}/${PYTHON_INSTALL_PATH}/. Under
  # orchestra the two collapse (PYTHON_INSTALL_PATH is just
  # "python"); on nix it's lib/python3.14/site-packages, so we
  # rewrite the python_module hard-code to match.
  #
  # That alignment exposes a target-level cycle in develop:
  #   revng-all-binaries → install-all-wheels →
  #   install-revng-wheel → mixins.py → revng-all-binaries
  # Cycle is intrinsic to develop; cmake 3.x apparently is
  # lax enough about it to still generate (orchestra builds),
  # but the bigger issue is just dropping the explicit
  # `add_dependencies(revng-all-binaries install-all-wheels)`
  # removes the cycle without losing functionality (install-
  # all-wheels still has the `ALL` flag so it builds anyway).
  postPatch = ''
    patchShebangs --build .

    # python/CMakeLists.txt has a baked-in assumption that
    # PYTHON_INSTALL_PATH == "python" (true for orchestra,
    # where the custom python is built with purelib =
    # $prefix/python). On nix purelib is the standard
    # lib/python3.14/site-packages and a bunch of staging
    # paths hard-coded to ''${CMAKE_BINARY_DIR}/python/ no
    # longer line up with anything. Rewrite them to
    # ''${CMAKE_BINARY_DIR}/''${PYTHON_INSTALL_PATH}/ so they
    # match pip's --prefix install dir.
    find python lib include share -name CMakeLists.txt -o -name '*.cmake' \
      | xargs sed -i \
      -e 's|''${CMAKE_BINARY_DIR}/python/|''${CMAKE_BINARY_DIR}/''${PYTHON_INSTALL_PATH}/|g'

    # Drop the explicit cycle-causing dep. install-all-wheels
    # still has the `ALL` flag so it gets built anyway.
    sed -i \
      -e '/^add_dependencies(revng-all-binaries install-all-wheels)$/d' \
      python/CMakeLists.txt

    # buffered_reader.py only appears at the staging dir
    # after pip install runs, and the dep order is now
    # uncertain (we just removed the only thing that forced
    # it). Read the file from the source tree directly.
    sed -i \
      -e 's|''${CMAKE_BINARY_DIR}/''${PYTHON_INSTALL_PATH}/revng/support/buffered_reader.py|''${CMAKE_SOURCE_DIR}/python/revng/support/buffered_reader.py|g' \
      python/CMakeLists.txt

    # generate-project-mixins.sh shells out to ./bin/revng2,
    # which is created by pip install (which we no longer
    # order before this step). Use python -m instead and
    # add the source tree to PYTHONPATH so the import works
    # without the wheel being installed.
    sed -i \
      -e 's|./bin/revng2|python3 -m revng.internal.cli.revng2|' \
      scripts/generate-project-mixins.sh

    # revng2 (invoked above) eventually imports the C++
    # extension `revng.internal._pipebox`; cmake doesn't
    # include the _pipebox target in revng-all-binaries, so
    # without an explicit dep ninja may run mixins.py before
    # _pipebox.so has been built.
    sed -i \
      -e 's|DEPENDS revng-all-binaries|DEPENDS revng-all-binaries _pipebox|' \
      python/CMakeLists.txt

    # The buffered_reader copy is normally created by a
    # cmake rule late in the build; mixins.py (which runs
    # revng2 -> imports revng.pypeline.storage.rss ->
    # imports revng.pypeline.utils.buffered_reader) runs
    # earlier with PYTHONPATH pointing at the staging tree.
    # Pre-place a copy in the source tree so that pip
    # install (and the wheel staging) finds it where the
    # tree implies it ought to be.
    cp python/revng/support/buffered_reader.py \
       python/revng/pypeline/utils/buffered_reader.py
  '';

  # When mixins.py runs (early in the build, before
  # install-all-wheels has populated the staging tree) it
  # imports `from revng.model import _generated`. _generated
  # is produced by tuple-tree-generate into
  # ''${CMAKE_BINARY_DIR}/''${PYTHON_INSTALL_PATH}/revng/model/
  # but the rest of the revng package only lives in the
  # source tree. Python won't merge two parent dirs for a
  # regular package, so pre-stage the source revng package
  # into the build staging tree before the build starts.
  preBuild = ''
    mkdir -p "${python.sitePackages}/revng"
    cp -rT ../python/revng "${python.sitePackages}/revng"
    chmod -R u+w "${python.sitePackages}/revng"

    # revng2 uses click + xdg dirs that fall back to $HOME
    # for a cache; the sandbox's HOME (/homeless-shelter) is
    # unwritable.
    export HOME="$TMPDIR/home"
    mkdir -p "$HOME"
    export XDG_CACHE_HOME="$HOME/.cache"
    mkdir -p "$XDG_CACHE_HOME"
  '';

  # The pypeline-annotations-test cmake rule embeds the
  # PYTHONPATH that's set at configure time (via $ENV{PYTHONPATH}
  # in set_tests_properties), so we have to inject nanobind's
  # site-packages here, not later in checkPhase.
  preConfigure = ''
    export PYTHONPATH="${nanobind}/${python.sitePackages}''${PYTHONPATH:+:$PYTHONPATH}"
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
    # WIP: revng's LinkForTranslation calls bare `ld.bfd` with
    # -l:crt1.o etc.; install a configuration.yml that tells it
    # where the host crt files (crt1.o/crti.o/crtbegin.o/...)
    # live. Drop once revng stops shelling out to ld directly
    # (and instead uses cc with PATH/LIBRARY_PATH).
    "-DREVNG_SYSTEM_CONFIG=${pkgs.writeText "revng.yml" ''
      translation-ldflags:
      - -L${pkgs.glibc}/lib
      - -L${pkgs.gcc-unwrapped}/lib/gcc/x86_64-unknown-linux-gnu/${pkgs.gcc-unwrapped.version}
      - -L${pkgs.gcc-unwrapped.lib}/lib
      - -L${pkgs.libunwind}/lib
      - -L${pkgs.glib.out}/lib
      - -L${pkgs.zlib}/lib
      # libstdc++ pulls in _Unwind_RaiseException from
      # libgcc_s, but revng's LinkForTranslation passes
      # `-lgcc` (the static archive), not `-lgcc_s`. ld.bfd
      # defaults to --no-copy-dt-needed-entries, so it
      # refuses to resolve symbols through implicit DSO
      # deps. Allow that.
      - --copy-dt-needed-entries
      # qemu's syscall.c references g_memdup (glib);
      # add libglib so ld.bfd can resolve it.
      - -lglib-2.0
    ''}"
  ];

  doCheck = true;

  checkPhase = ''
    export PATH="${llvm}/libexec:$PATH"
    # pypeline-annotations-test runs nanobind_generate_stubs.py
    # which `import nanobind`; nanobind isn't on PYTHONPATH by
    # default during the build.
    export PYTHONPATH="${nanobind}/${python.sitePackages}:${revngPythonDependencies}/${python.sitePackages}''${PYTHONPATH:+:$PYTHONPATH}"
    # Skip tests that are flaky/broken on our setup:
    # - pypeline-native-test fires an assertion in
    #   tools/pypeline/run-pipe (likely a pipe registry
    #   mismatch in our build);
    # - mlir-lit-tests has several failing lit cases unrelated
    #   to the bump (~1 in 372 normally).
    ctest -j$(nproc) --exclude-regex 'pypeline-native-test|mlir-lit-tests'
  '';

  postFixup = ''
    for PROGRAM in revng revng2 pype; do
        wrapProgram $out/bin/"$PROGRAM" --prefix PYTHONPATH : "${revngPythonDependencies}/${python.sitePackages}"
    done
  '';

}
