{ pkgs }:
# `pkgsCross.mingwW64` with the libcxxStdenv triplet patches we need
# for every C++ Windows derivation (windows/llvm, qemu, boost,
# aws-sdk-cpp, nanobind, revng). Used to be inlined in every file;
# each copy created a fresh overlay scope so nix saw distinct
# derivations and rebuilt boost/llvm/clang-wrapper per consumer.
# Shared here so consumers reuse one store path per dep.
#
# Three fixes layered on `llvmPackages_21`:
#
# 1. `compiler-rt-no-libc` cmake assumes a working libc compile test
#    because the bootstrap wrapper exposes `libc = preLibcHeaders`
#    (mingw_w64_headers, headers-only). Force the `!haveLibc` cmake
#    flags directly since `overrideAttrs` can't flip the `haveLibc`
#    attr itself.
#
# 2. `libcxxClang` (the wrapper libcxxStdenv consumers see) doesn't
#    tell clang to use compiler-rt as the runtime lib, so the mingw
#    clang driver defaults to `-rtlib=libgcc` and asks the linker for
#    `-lgcc -lgcc_eh`. Inject `-rtlib=compiler-rt` + `-B${compiler-rt}
#    /lib`, plus `-Qunused-arguments` to silence the unused-link-flag
#    warning that some compile-tests escalate to a hard error.
#
# 3. `libcxx` itself needs libunwind for `_Unwind_Resume` references,
#    plus two COFF-export fixes (see comments inside) so libc++.dll
#    exports operator new/delete with the right symbol type.
let
  windowsCross = pkgs.pkgsCross.mingwW64.extend (
    _final: prev: {
      llvmPackages_21 = prev.llvmPackages_21.overrideScope (
        _lFinal: lPrev: {
          compiler-rt-no-libc = lPrev.compiler-rt-no-libc.overrideAttrs (old: {
            cmakeFlags = (old.cmakeFlags or [ ]) ++ [
              "-DCMAKE_C_COMPILER_WORKS=TRUE"
              "-DCMAKE_CXX_COMPILER_WORKS=TRUE"
              "-DCOMPILER_RT_BAREMETAL_BUILD=TRUE"
              "-DCOMPILER_RT_BUILD_XRAY=FALSE"
              "-DCOMPILER_RT_BUILD_LIBFUZZER=FALSE"
              "-DCOMPILER_RT_BUILD_MEMPROF=FALSE"
              "-DCOMPILER_RT_BUILD_ORC=FALSE"
              "-DCOMPILER_RT_BUILD_PROFILE=FALSE"
              "-DCOMPILER_RT_BUILD_SANITIZERS=FALSE"
            ];
          });
          libcxxClang = lPrev.libcxxClang.overrideAttrs (old: {
            postFixup = (old.postFixup or "") + ''
              echo "-rtlib=compiler-rt" >> $out/nix-support/cc-cflags
              echo "-B${_lFinal.compiler-rt}/lib" >> $out/nix-support/cc-cflags
              echo "-Qunused-arguments" >> $out/nix-support/cc-cflags
            '';
          });
          libcxx = lPrev.libcxx.overrideAttrs (old: {
            buildInputs = (old.buildInputs or [ ]) ++ [ _lFinal.libunwind ];
            preConfigure = (old.preConfigure or "") + ''
              export NIX_LDFLAGS_x86_64_w64_mingw32="''${NIX_LDFLAGS_x86_64_w64_mingw32:-} -lunwind"
              export NIX_CFLAGS_COMPILE_x86_64_w64_mingw32="''${NIX_CFLAGS_COMPILE_x86_64_w64_mingw32:-} --unwindlib=libunwind"
            '';
            # Two libcxx COFF/CMakeLists fixes (cd runtimes/ has already
            # happened in the upstream postPatch, so paths are relative
            # to runtimes/):
            #
            # (A) Operators must be exported from `libc++.dll` so other
            #     DLLs can import them via `libc++.dll.a`. The upstream
            #     `-fvisibility=hidden` on libcxx targets stops auto-
            #     export on COFF. Flip to `default`.
            # (B) clang's `__attribute__((weak))` (the `_LIBCPP_WEAK`
            #     macro) emits a weak-external whose default alias is
            #     `__clang_call_terminate`, not the function body. The
            #     DLL export table then sees the operator as undefined
            #     and the link fails. Strip `_LIBCPP_WEAK` so the
            #     operators are strong globals. We lose user-overridable
            #     new/delete on Windows — acceptable for our self-
            #     contained builds.
            postPatch = (old.postPatch or "") + ''
              sed -i 's|-fvisibility=hidden|-fvisibility=default|' \
                ../libcxx/CMakeLists.txt
              sed -i 's|_LIBCPP_OVERRIDABLE_FUNCTION(type, name, arglist) _LIBCPP_WEAK type name arglist|_LIBCPP_OVERRIDABLE_FUNCTION(type, name, arglist) type name arglist|' \
                ../libcxx/src/include/overridable_function.h
              sed -i 's|^_LIBCPP_WEAK |/* _LIBCPP_WEAK */ |' \
                ../libcxxabi/src/stdlib_new_delete.cpp \
                ../libcxx/src/verbose_abort.cpp
            '';
          });
        }
      );
    }
  );
in
windowsCross
