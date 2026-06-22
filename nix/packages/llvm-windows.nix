{ pkgs, python, llvm, windowsCross }:
# Cross-build of revng's LLVM fork targeting x86_64-w64-mingw32.
# First iteration: llvm-core only (no clang/mlir/lld in PROJECTS,
# X86 target only). Toolchain is the cross libcxxStdenv from
# `windowsCross.llvmPackages_21` (shared overlay — see
# `windows-cross.nix` for the actual fixes).
#
# `llvm` (the host build) is reused to provide `llvm-tblgen` so the
# cross configure doesn't try to bootstrap a host tblgen on its own.
let
  windowsStdenv = windowsCross.llvmPackages_21.libcxxStdenv;
in
windowsStdenv.mkDerivation {
  name = "windows-llvm";

  src = pkgs.fetchFromGitHub {
    owner = "revng";
    repo = "llvm-project";
    # Keep in sync with nix/packages/llvm.nix — both must point at the
    # same revng/llvm-project commit so the cross-built libLLVM matches
    # the host one revng's passes are linked against.
    rev = "e828226b25c569d733c801ddf69f555e9b0bc50e";
    hash = "sha256-K57CoXCk2eFluz6iS9PWCEQBGhPmCmlWpKUwetkrXU4=";
  };

  nativeBuildInputs = (with pkgs; [
    cmake
    ninja
    # `-fuse-ld=lld` needs `ld.lld` on PATH. The cross-mingw clang
    # wrapper otherwise defaults to binutils-mingw ld.
    llvmPackages_21.lld
  ]) ++ [
    python
    # host llvm provides tblgen tools used during the cross configure
    llvm
  ];

  buildInputs = [
    windowsCross.zlib
    windowsCross.zstd
    windowsCross.windows.pthreads
  ];

  cmakeFlags = [
    "-GNinja"

    "-DCMAKE_C_FLAGS=-O2"
    # CMAKE_CXX_FLAGS is set in preConfigure via cmakeFlagsArray —
    # the value contains spaces (orchestra's `feature/windows` combo:
    # `-fvisibility=hidden -fvisibility-inlines-hidden -fuse-ld=lld`)
    # which gets word-split if placed in cmakeFlags.
    "-DCMAKE_BUILD_TYPE=Debug"

    "-DCMAKE_INSTALL_BINDIR=libexec"

    "-DLLVM_INSTALL_UTILS=ON"
    "-DLLVM_ENABLE_DUMP=ON"
    "-DLLVM_ENABLE_TERMINFO=OFF"
    "-DCMAKE_CXX_STANDARD=20"
    "-DLLVM_ENABLE_Z3_SOLVER=OFF"
    "-DLLVM_ENABLE_ZLIB=ON"
    # libedit is *nix-only (uses termios); nixpkgs provides a cross
    # variant but it can't link on a real mingw target. Drop it for
    # now — revng doesn't use the lldb-style REPL paths it gates.
    "-DLLVM_ENABLE_LIBEDIT=OFF"
    "-DLLVM_ENABLE_LIBXML2=OFF"
    "-DLLVM_ENABLE_ZSTD=ON"

    # Per-component shared libs (orchestra's approach for windows/
    # llvm). Combined with the -fvisibility=hidden + -fuse-ld=lld
    # flags above, this is the only LLVM-on-Windows configuration
    # that links cleanly without resorting to LLVM_BUILD_LLVM_DYLIB
    # (which produces both static .a and import .dll.a — tools then
    # link both and ld errors with multiple-definition).
    "-DBUILD_SHARED_LIBS=ON"
    # Without this, LLVM-on-Windows DLLs only export the handful of
    # symbols explicitly tagged `LLVM_EXTERNAL_VISIBILITY` (e.g.
    # `LLVMInitializeX86TargetInfo`). Consumers like revng need
    # `llvm::Type`, `llvm::Value`, etc — `WINDOWS_EXPORT_ALL_SYMBOLS`
    # tells CMake to post-process each .obj file with `__create_def`
    # and feed the resulting `.def` to the linker, so every extern
    # symbol in each component's translation units becomes an export.
    # Each DLL has its own symbol set, so this does not produce the
    # multi-def errors we saw with `LLVM_BUILD_LLVM_DYLIB`.
    "-DCMAKE_WINDOWS_EXPORT_ALL_SYMBOLS=ON"
    # Match `llvm.nix` (the Linux build). revng's `lib/Yield` calls
    # `llvm_map_components_to_libnames(... AArch64Disassembler ...)` —
    # the call errors out at configure time if the target is omitted,
    # so we can't restrict to X86 here.
    "-DLLVM_TARGETS_TO_BUILD=AArch64;ARM;Mips;SystemZ;X86"
    # revng's top-level CMakeLists.txt does find_package(MLIR) and
    # find_package(Clang). Orchestra's 2021 `feature/windows`
    # shipped clang only (revng didn't need MLIR then); we keep MLIR
    # and LLD because the current revng links against both.
    "-DLLVM_ENABLE_PROJECTS=clang;mlir;lld"
    # Don't compile examples/tests we'd never use.
    "-DLLVM_BUILD_EXAMPLES=OFF"
    "-DLLVM_INCLUDE_EXAMPLES=OFF"
    "-DLLVM_BUILD_TESTS=OFF"
    "-DLLVM_INCLUDE_TESTS=OFF"

    # Use the host build's tablegen + llvm-config so the cross build
    # doesn't try to bootstrap a host-runnable copy from cross sources.
    # `LLVM_NATIVE_TOOL_DIR` points at the directory containing every
    # NATIVE tool LLVM might want — it covers `llvm-config` (otherwise
    # cmake spawns a sub-build under `NATIVE/` using our mingw clang
    # which can't compile linux headers like `uid_t`) plus tblgen.
    "-DLLVM_TABLEGEN=${llvm}/libexec/llvm-tblgen"
    "-DLLVM_NATIVE_TOOL_DIR=${llvm}/libexec"
  ];

  # Inject the cross-mingw libunwind explicitly. The wrapper-level
  # `--unwindlib=libunwind` doesn't auto-add `-lunwind` on the mingw
  # clang driver, so we plumb it via NIX_LDFLAGS_x86_64_w64_mingw32
  # with an explicit `-L` to the cross-mingw libunwind.
  NIX_LDFLAGS_x86_64_w64_mingw32 =
    "-L${windowsCross.llvmPackages_21.libunwind}/lib -lunwind";

  # Same libc++21 explicit-ctor workaround as the host build.
  postPatch = ''
    sed -i 's|SpecialCaseList::createOrDie({{ClIgnorelist}},|SpecialCaseList::createOrDie({static_cast<std::string>(ClIgnorelist)},|' \
      llvm/tools/sancov/sancov.cpp
  '';

  preConfigure = ''
    # orchestra's `feature/windows` had `-fvisibility=hidden
    # -fvisibility-inlines-hidden` here, but on our LLVM 16 base that
    # hides internal-but-cross-DLL symbols (e.g. itanium_demangle::
    # parse_discriminator) that libLLVMSupport.dll needs from
    # libLLVMDemangle.dll. Drop visibility for now; rely on
    # -fuse-ld=lld for the right COFF export behaviour.
    cmakeFlagsArray+=(
      "-DCMAKE_CXX_FLAGS=-O2 -fuse-ld=lld"
    )
    cd llvm
  '';

}
