{ pkgs,
  # The Linux-host python env reused for codegen: tuple-tree-gen,
  # mixin-gen, etc. run on the BUILD host during configure, so we need
  # yaml/jinja2 here even though the Windows install never ships them.
  revngPythonDependencies,
  # Host (Linux) LLVM — used as the source of native `mlir-tblgen`,
  # `llvm-tblgen` etc., since the cross-LLVM only ships Windows .exe's
  # that can't be exec'd at build time.
  llvm,
  windowsCross,
  # Windows deps wired in by `default.nix` from the scope so we don't
  # have to re-callPackage them (each has a slightly different arg
  # list — python, inputs, etc. — that's easier to plumb at the
  # scope layer than to re-resolve here).
  windowsPython,
  windowsLlvm,
  windowsQemu,
  windowsBoost,
  windowsAwsSdkCpp,
  windowsNanobind,
}:
# Cross-build of revng targeting x86_64-w64-mingw32. Pulls together
# every `windows/*` dependency we've already wired up and turns on
# the `REVNG_CROSS_BUILD` CMakeLists.txt knob so the build skips
# steps that would otherwise need to run the just-built Windows
# binaries (helper-module annotation, mkdocs, clang-format config,
# tests). The resulting tree has the C++ libs + .exe's and the
# nanobind-built python extensions, but not the artifacts that
# require running revng to produce them.
let
  windowsStdenv = windowsCross.llvmPackages_21.libcxxStdenv;
in
windowsStdenv.mkDerivation {
  name = "windows-revng";

  src = pkgs.lib.cleanSourceWith {
    src = ../..;
    filter =
      path: type:
      let
        base = baseNameOf path;
        prefix = toString ../..;
        rel = pkgs.lib.removePrefix (prefix + "/") (toString path);
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
        || rel == "share/revng/test"
        || pkgs.lib.hasPrefix "share/revng/test/" rel
      );
  };

  nativeBuildInputs = (with pkgs; [
    cmake
    ninja
    pkg-config
    git
    jq
    unzip
    # `-fuse-ld=lld` needs `ld.lld` on PATH. The lld-produced .dll.a
    # import libs in `windows/llvm` are not parseable by binutils ld,
    # so revng must link with lld as well.
    llvmPackages_21.lld
  ]) ++ [
    # Brings python314 with yaml/jinja2/... for codegen scripts.
    revngPythonDependencies
  ];

  buildInputs = [
    windowsAwsSdkCpp
    windowsBoost
    windowsCross.libarchive
    windowsCross.sqlite
    windowsCross.zlib
    windowsCross.zstd
    # `lib/Lift` includes `qemu/libtcg/libtcg.h` — the Linux build
    # finds it because the Linux qemu sits in buildInputs and nixpkgs
    # adds its `include/` to the C search path. Mirror that here.
    windowsQemu
    # `lib/Pipebox` + `include/revng/PipeboxCommon/Helpers/Registrars.h`
    # include `nanobind/nanobind.h`; same story.
    windowsNanobind
  ];

  postPatch = ''
    patchShebangs --build .

    # `python/CMakeLists.txt` adds the `_pipebox` python C extension as
    # a SHARED library with `PREFIX ""` (so it's `_pipebox` not
    # `lib_pipebox`). On Windows two CMake quirks bite:
    #   1. The default SUFFIX for SHARED is `.dll`, but the wheel
    #      manifest, the ninja rules and the python-side
    #      `import revng.internal._pipebox` all expect the Linux-style
    #      `.so` — force the suffix.
    #   2. CMake puts the actual .dll/.so in RUNTIME_OUTPUT_DIRECTORY
    #      (not LIBRARY_OUTPUT_DIRECTORY which only governs the import
    #      lib on Windows). Mirror the LIBRARY_ path into RUNTIME_ so
    #      the file ends up where the wheel rule looks for it.
    #
    # Loading a Windows DLL named `.so` from CPython is the python-
    # runtime side's problem (probably needs an explicit-loader shim);
    # for now we just want the cross-build to complete.
    sed -i 's|PREFIX ""|PREFIX "" SUFFIX ".so"|' python/CMakeLists.txt

    # MLIRConfig.cmake sets MLIR_TABLEGEN_EXE to the imported target
    # `mlir-tblgen`, whose IMPORTED_LOCATION is the cross-compiled
    # `mlir-tblgen.exe` and can't be exec'd at build time. Same for
    # LLVM_TABLEGEN_EXE / CLANG_TABLEGEN_EXE. Override the imported-
    # target locations after find_package(...) so revng's `mlir_tablegen`
    # / `tablegen` calls invoke the host Linux binaries. We rely on
    # the consumer passing -D{MLIR,LLVM,CLANG}_TABLEGEN_EXE_HOST=...
    # (set in cmakeFlags below).
    sed -i \
      -e 's|^find_package(MLIR REQUIRED CONFIG)$|&\nset_target_properties(mlir-tblgen PROPERTIES IMPORTED_LOCATION "''${MLIR_TABLEGEN_EXE_HOST}")\nset(MLIR_TABLEGEN_EXE "''${MLIR_TABLEGEN_EXE_HOST}")|' \
      -e 's|^find_package(LLVM REQUIRED CONFIG)$|&\nset_target_properties(llvm-tblgen PROPERTIES IMPORTED_LOCATION "''${LLVM_TABLEGEN_EXE_HOST}")\nset(LLVM_TABLEGEN_EXE "''${LLVM_TABLEGEN_EXE_HOST}")|' \
      -e 's|^find_package(Clang REQUIRED CONFIG)$|&\nset_target_properties(clang-tblgen PROPERTIES IMPORTED_LOCATION "''${CLANG_TABLEGEN_EXE_HOST}")\nset(CLANG_TABLEGEN_EXE "''${CLANG_TABLEGEN_EXE_HOST}")|' \
      CMakeLists.txt

    # `lib/Support/Configuration.cpp` uses POSIX glob() to expand
    # ld.so.conf-style `include` directives. mingw has no glob.h; on
    # Windows the runtime never sees those configs anyway. Inject a
    # tiny no-match stub so the file compiles without touching its
    # Linux behaviour.
    sed -i 's|^#include "glob.h"$|#if defined(_WIN32)\n#include <stddef.h>\ntypedef struct { size_t gl_pathc; char **gl_pathv; } glob64_t;\n#define GLOB_NOMATCH 1\n#define GLOB_NOSPACE 2\n#define GLOB_ABORTED 3\nstatic inline int glob64(const char*, int, void*, glob64_t* p) { p->gl_pathc = 0; p->gl_pathv = 0; return GLOB_NOMATCH; }\nstatic inline void globfree64(glob64_t*) {}\n#else\n#include "glob.h"\n#endif|' \
      lib/Support/Configuration.cpp

    # `-Wunnecessary-virtual-specifier` (new in clang 21) trips on
    # LLVM 21's IR/ConstantFolder.h and Analysis/TargetFolder.h
    # (virtual void anchor() in a `final` class). revng's CMakeLists
    # turns on `-Wall -Wextra -Werror` AFTER the user-supplied
    # CMAKE_CXX_FLAGS, which re-enables our `-Wno-` and breaks the
    # build. Append the suppression at the END of the warning block
    # so it survives `-Wall`.
    sed -i \
      '/^add_flag_if_available("-Wno-unused-but-set-variable")$/a\
add_flag_if_available("-Wno-unnecessary-virtual-specifier")' \
      CMakeLists.txt
    cat >> python/CMakeLists.txt <<'EOF'

# Windows cross-build only: pin `_pipebox` to the same path that the
# Linux build's LIBRARY_OUTPUT_DIRECTORY uses. On Windows, CMake puts
# SHARED libs in RUNTIME_OUTPUT_DIRECTORY (the .dll.a import lib goes
# in LIBRARY_OUTPUT_DIRECTORY) so the file otherwise lands under
# ''${CMAKE_BINARY_DIR}/ rather than the python site-packages path.
set_target_properties(
  _pipebox PROPERTIES RUNTIME_OUTPUT_DIRECTORY
  "''${CMAKE_BINARY_DIR}/''${PYTHON_INSTALL_PATH}/revng/internal")
EOF
  '';

  cmakeFlags = [
    "-GNinja"
    "-DREVNG_CROSS_BUILD=ON"
    "-DCMAKE_CXX_STANDARD=20"
    "-DCMAKE_C_FLAGS=-O2"
    "-DCMAKE_CXX_FLAGS=-O2"
    # Linker flags are set in preConfigure via cmakeFlagsArray —
    # the value has spaces (`-fuse-ld=lld -L${windowsLlvm}/libexec`)
    # which CMake otherwise word-splits if passed in cmakeFlags.
    "-DCMAKE_BUILD_TYPE=Debug"
    "-DLLVM_DIR=${windowsLlvm}/lib/cmake/llvm"
    "-DMLIR_DIR=${windowsLlvm}/lib/cmake/mlir"
    "-DClang_DIR=${windowsLlvm}/lib/cmake/clang"
    "-DLIBTCG_DIR=${windowsQemu}"

    # MLIR/LLVM CMake configs store the absolute path of the tablegen
    # binary they were built with — `mlir-tblgen.exe` & co. Codegen
    # calls during the revng build can't exec the Windows binary on
    # a Linux host, so feed the host (Linux) tablegen paths into our
    # postPatch override. _HOST suffix avoids collisions with the
    # `_EXE` variables that find_package(MLIR/LLVM/Clang) overwrites.
    "-DMLIR_TABLEGEN_EXE_HOST=${llvm}/libexec/mlir-tblgen"
    "-DLLVM_TABLEGEN_EXE_HOST=${llvm}/libexec/llvm-tblgen"
    "-DCLANG_TABLEGEN_EXE_HOST=${llvm}/libexec/clang-tblgen"

    # Cross Python hints. Use the BUILD-host python as the
    # interpreter (FindPython runs it to introspect site-packages /
    # version), but feed the cross-built include + import library
    # for the actual link.
    "-DPython3_INCLUDE_DIR=${windowsPython}/include/python3.14"
    "-DPython3_LIBRARY=${windowsPython}/lib/libpython3.14.dll.a"
    "-DPython3_EXECUTABLE=${pkgs.python314}/bin/python3.14"
  ];

  preConfigure = ''
    # Use lld (see nativeBuildInputs note) — bfd ld can't read the
    # `.dll.a` import libs that lld produced for windows/llvm.
    linkerFlags="-fuse-ld=lld"
    cmakeFlagsArray+=(
      "-DCMAKE_EXE_LINKER_FLAGS=$linkerFlags"
      "-DCMAKE_SHARED_LINKER_FLAGS=$linkerFlags"
      "-DCMAKE_MODULE_LINKER_FLAGS=$linkerFlags"
    )
  '';

  # Surface every Windows-portability error in one pass instead of
  # bouncing through nix-build for each individual failure.
  ninjaFlags = [ "-k0" ];

  doCheck = false;
}
