{ pkgs, windowsCross }:
# Cross-build of revng's nanobind fork targeting x86_64-w64-mingw32,
# linked against the cross-built `windows/python` we ship.
#
# Mirrors `nix/packages/nanobind.nix` (same source + cmake flags) but:
#   - drops the `python.pkgs.toPythonModule` wrapper — that's
#     Python-package-set machinery (interpretation in `withPackages`)
#     which doesn't carry over to a cross-mingw python that can't be
#     imported on the build host anyway
#   - uses the shared `windowsCross` libcxxStdenv overlay (see
#     `windows-cross.nix`)
#   - feeds CMake hand-curated `Python_*` hints because FindPython
#     can't introspect the cross-target interpreter from the build host
let
  windowsPython = (import ./python-windows.nix) { inherit pkgs; };
  windowsStdenv = windowsCross.llvmPackages_21.libcxxStdenv;
in
windowsStdenv.mkDerivation {
  name = "windows-nanobind";

  src = pkgs.fetchFromGitHub {
    owner = "revng";
    repo = "nanobind";
    fetchSubmodules = true;
    rev = "a111828dd36d1ce3c8443d2bfc74ac292169a0f3";
    hash = "sha256-sxEehWW+NdoWl+EO/uZ1CzD39Fibsw/XomkUKrFDsQA=";
  };

  postPatch = ''
    # Same `PYTHON_INSTALL_PATH` fix as the host nanobind — point at
    # ${"$"}{python.sitePackages} inside ${"$"}out so the install stays put.
    substituteInPlace standalone/CMakeLists.txt \
      --replace 'DESTINATION "''${CMAKE_INSTALL_PREFIX}/''${PYTHON_INSTALL_PATH}/nanobind"' \
                'DESTINATION "''${CMAKE_INSTALL_PREFIX}/${windowsPython.sitePackages}/nanobind"'

    # exception_shim.h calls `fprintf(stderr, ...)` without including
    # `<cstdio>` itself. glibc pulls cstdio in transitively from other
    # system headers; mingw does not, so consumers of nanobind on
    # Windows hit "use of undeclared identifier 'stderr'". Add the
    # include.
    sed -i '1i #include <cstdio>' include/nanobind/exception_shim.h
  '';

  nativeBuildInputs = with pkgs; [
    cmake
    ninja
    # Build-host python (3.14) for CMake FindPython — the cross
    # interpreter `python.exe` can't execute on linux.
    python314
  ];

  cmakeFlags = [
    "-GNinja"
    "-DCMAKE_CXX_STANDARD=20"
    "-DBUILD_SHARED_LIBS=ON"

    # Cross-Python hints: FindPython3 can't probe the mingw interpreter
    # from a Linux build host, so feed the include dir + import library
    # directly. The cross-built `windows/python` ships both under its
    # conventional Unix-style layout.
    "-DPython_INCLUDE_DIR=${windowsPython}/include/python3.14"
    "-DPython_LIBRARY=${windowsPython}/lib/libpython3.14.dll.a"
    "-DPython_EXECUTABLE=${pkgs.python314}/bin/python3.14"
    "-DPython3_INCLUDE_DIR=${windowsPython}/include/python3.14"
    "-DPython3_LIBRARY=${windowsPython}/lib/libpython3.14.dll.a"
    "-DPython3_EXECUTABLE=${pkgs.python314}/bin/python3.14"
  ];

  preConfigure = "cd standalone";
}
