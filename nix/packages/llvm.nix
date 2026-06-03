{ pkgs, ccacheStdenv, python }:
# Build our LLVM fork
ccacheStdenv.mkDerivation {
  name = "llvm";

  src = pkgs.fetchFromGitHub {
    owner = "revng";
    repo = "llvm-project";
    rev = "092c88c578306e6aa96cf28f9f4c4c33065ccce7";
    hash = "sha256-dvXiG/Qkng9y/RbRgBLeSlgz9v/WNy+9Rz+NMifxZ0U=";
  };

  nativeBuildInputs = (with pkgs; [
    cmake
    ninja
    zlib
    libedit
  ]) ++ [ python ];

  cmakeFlags = [
    "-GNinja"

    "-DCMAKE_C_FLAGS=-O2"
    "-DCMAKE_CXX_FLAGS=-O2"
    "-DCMAKE_BUILD_TYPE=Debug"

    "-DCMAKE_INSTALL_BINDIR=libexec"

    "-DLLVM_INSTALL_UTILS=ON"
    "-DLLVM_ENABLE_DUMP=ON"
    "-DLLVM_ENABLE_TERMINFO=OFF"
    "-DCMAKE_CXX_STANDARD=20"
    "-DLLVM_ENABLE_Z3_SOLVER=OFF"
    "-DLLVM_ENABLE_ZLIB=ON"
    "-DLLVM_ENABLE_LIBEDIT=ON"
    "-DLLVM_ENABLE_LIBXML2=OFF"
    "-DLLVM_ENABLE_ZSTD=OFF"

    "-DBUILD_SHARED_LIBS=ON"
    "-DLLVM_ENABLE_PROJECTS=clang;mlir"
    "-DLLVM_TARGETS_TO_BUILD=AArch64;ARM;Mips;SystemZ;X86"
    "-DCMAKE_CXX_FLAGS=-Wno-global-constructors"
  ];

  # sancov.cpp uses `{{ClIgnorelist}}` to build a vector<string>;
  # the inner brace tries to copy-construct std::string from a
  # cl::opt<std::string>, which fails under libc++21 because the
  # templated basic_string(const _Tp&) ctor is now `explicit`.
  # Force a direct-init conversion via static_cast.
  postPatch = ''
    sed -i 's|SpecialCaseList::createOrDie({{ClIgnorelist}},|SpecialCaseList::createOrDie({static_cast<std::string>(ClIgnorelist)},|' \
      llvm/tools/sancov/sancov.cpp
  '';

  preConfigure = "cd llvm";

}
