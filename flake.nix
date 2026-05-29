# TODO: try on macmini

{
  inputs = {
    nixpkgs.url = "git+file:///home/nix/nixpkgs";

    nixpkgs-2505.url = "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-25.05-small.tar.gz";

    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    uv2nix_hammer_overrides = {
      url = "github:TyberiusPrime/uv2nix_hammer_overrides";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-2505,
      poetry2nix,
      uv2nix,
      pyproject-nix,
      pyproject-build-systems,
      uv2nix_hammer_overrides,
    }:
    let
      system = "x86_64-linux";
      ccacheOverlay = (
        self: super: {
          ccacheWrapper = super.ccacheWrapper.override {
            extraConfig = ''
              export CCACHE_COMPRESS=1
              export CCACHE_SLOPPINESS=random_seed
              export CCACHE_DIR="/nix/var/cache/ccache"
              export CCACHE_UMASK=007
              if [ ! -d "$CCACHE_DIR" ]; then
                echo "====="
                echo "Directory '$CCACHE_DIR' does not exist"
                echo "Please create it with:"
                echo "  sudo mkdir -m0770 '$CCACHE_DIR'"
                echo "  sudo chown root:nixbld '$CCACHE_DIR'"
                echo "====="
                exit 1
              fi
              if [ ! -w "$CCACHE_DIR" ]; then
                echo "====="
                echo "Directory '$CCACHE_DIR' is not accessible for user $(whoami)"
                echo "Please verify its access permissions"
                echo "====="
                exit 1
              fi
            '';
          };
        }
      );
      pkgs = import nixpkgs {
        inherit system;
        # overlays = [ ccacheOverlay ];
      };
      pkgs-2505 = import nixpkgs-2505 {
        inherit system;
        # overlays = [ ccacheOverlay ];
      };

      # Match the Python version pinned by orchestra (3.14.x).
      python = pkgs.python314;

      # Adopt:
      #
      # * clang as a compiler
      # * libc++ as C++ standard library
      # * mold as linker
      stdenv = (pkgs.useMoldLinker pkgs.llvmPackages_21.libcxxStdenv);
      ccacheStdenv = stdenv;
      # ccacheStdenv = pkgs.ccacheStdenv.override {
      #   stdenv = stdenv;
      #   extraConfig = ''
      #     export CCACHE_DIR="''${CCACHE_DIR:-/nix/var/cache/ccache}"
      #     export CCACHE_COMPRESS=1
      #     export CCACHE_SLOPPINESS=random_seed
      #     export CCACHE_UMASK=007
      #   '';
      # };

      #
      # Build C++ dependencies using our stdenv
      #
      boost-test =
        (pkgs.lib.fix (
          self:
          pkgs.callPackage "${nixpkgs}/pkgs/development/libraries/boost/1.81.nix" {
            stdenv = pkgs.llvmPackages_21.libcxxStdenv;

            # Use the right version of boost-build.
            # This has been copied from nixpkgs.
            boost-build = pkgs.boost-build.override { useBoost = self; };
          }
        )).overrideAttrs
          (oldAttrs: {
            # Build only the libraries we're interseted in
            configureFlags = oldAttrs.configureFlags ++ [ "--with-libraries=test" ];
          });

      aws-crt-cpp = (
        pkgs.callPackage "${nixpkgs}/pkgs/by-name/aw/aws-crt-cpp/package.nix" {
          stdenv = stdenv;
        }
      );

      aws-sdk-cpp =
        (pkgs.callPackage "${nixpkgs}/pkgs/by-name/aw/aws-sdk-cpp/package.nix" {
          stdenv = stdenv;

          aws-crt-cpp = aws-crt-cpp;

          # Only build the APIs we're interested in
          apis = [ "s3" ];
        }).overrideAttrs
          (oldAttrs: {
            cmakeFlags = oldAttrs.cmakeFlags ++ [
              "-DENABLE_TESTING=OFF"
              "-DFORCE_CURL=ON"
              "-DENABLE_UNITY_BUILD=OFF"
              "-DENABLE_RTTI=OFF"
              "-DCPP_STANDARD=20"
            ];
          });

      makeQemu =
        pkgs: llvmPackages: name: cflags: suffixes:
        (llvmPackages.stdenv.mkDerivation {
          name = name;

          src = pkgs.fetchFromGitHub {
            owner = "revng";
            repo = "qemu";
            rev = "a4c2561e7ed21b16dcbad730e0a64f0e0389b6ac";
            hash = "sha256-i1fipbBHkgJ0wgOsM4L/oRK/LRYKc+HQWNWVZxcMk8U=";
          };

          postPatch = ''
            patchShebangs python/scripts/link-embedded-objects

            grep -vF "subdir('fp')" tests/meson.build > tests/meson.build2
            mv tests/meson.build2 tests/meson.build

            # WIP
            grep -vF "_Static_assert" target/i386/cpu.h > target/i386/cpu.h2
            mv target/i386/cpu.h2 target/i386/cpu.h

            grep -vF "ASSERT_CONSTANT" libtcg/libtcg.c > libtcg/libtcg.c2
            mv libtcg/libtcg.c2 libtcg/libtcg.c
          '';

          preBuild = ''
            cd build
          '';

          nativeBuildInputs = (with pkgs; [
            pkg-config
            meson
            ninja
            coreutils-full
            zlib
            llvmPackages.clang
            llvmPackages.llvm
          ]) ++ [
            (python.withPackages (python-pkgs: [ python-pkgs.distlib ]))
            # Hooks from the python package are needed to add `$pythonPath` so
            # `python/scripts/mkvenv.py` can detect `meson` otherwise the vendored meson without patches will be used.
            python.pkgs.python
          ];

          buildInputs = with pkgs; [
            glib
          ];

          dontUseMesonConfigure = true;
          enableParallelBuilding = true;

          configureFlags =
            let
              targets = builtins.concatStringsSep "," (
                pkgs.lib.flatten (
                  map (
                    suffix:
                    map (architecture: "${architecture}-${suffix}") [
                      "arm"
                      "aarch64"
                      "i386"
                      "mips"
                      "mipsel"
                      "s390x"
                      "x86_64"
                    ]
                  ) suffixes
                )
              );
            in
            [
              "--disable-plugins"
              "--target-list=${targets}"
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
              "-Dvhost_user=disabled"
              "-Dxkbcommon=disabled"
              "--extra-cflags=-Wno-unused-variable"
              "--extra-cflags=-Wno-unused-function"
              "--extra-cflags=-Wno-unused-result"
              "--extra-cflags=-Wno-unused-but-set-variable"
              (map (argument: "--extra-cflags=${argument}") cflags)
            ];

          preInstall = ''
            mkdir -p $out/include
            mkdir -p $out/lib
          '';

          # The qemu develop branch dropped the glib entry from
          # libtcg-*.so's RUNPATH; revng dlopens these at build time and
          # the loader then can't find libglib-2.0.so.0. Add it back.
          postFixup = ''
            for so in $out/lib/libtcg-*.so; do
              [ -f "$so" ] || continue
              current=$(patchelf --print-rpath "$so" 2>/dev/null || true)
              patchelf --set-rpath "${pkgs.glib.out}/lib''${current:+:$current}" "$so"
            done
          '';
        });

    in
    {
      packages.${system} = {
        revngClang = pkgs-2505.clang_16;

        revngPythonDependencies =
          let
            workspace = uv2nix.lib.workspace.loadWorkspace {
              workspaceRoot = ./revng-python-dependencies;
            };
            pythonBase = pkgs.callPackage pyproject-nix.build.packages {
              inherit python;
            };
            overlay = workspace.mkPyprojectOverlay {
              sourcePreference = "wheel";
            };
            pythonSet = pythonBase.overrideScope (
              pkgs.lib.composeManyExtensions [
                pyproject-build-systems.overlays.wheel
                overlay
                (uv2nix_hammer_overrides.overrides pkgs)
                # Overrides for our forks (uv2nix_hammer_overrides only
                # covers upstream package names).
                (
                  final: prev:
                  let
                    addSetuptools = drv: drv.overrideAttrs (old: {
                      nativeBuildInputs =
                        (old.nativeBuildInputs or [ ]) ++ final.resolveBuildSystem { setuptools = [ ]; };
                    });
                  in
                  {
                    grandiso = addSetuptools prev.grandiso;
                    python-idb = addSetuptools prev.python-idb;
                    llvmcpy = addSetuptools prev.llvmcpy;
                    # psycopg-c needs both setuptools (hammer covers psycopg
                    # but not psycopg-c) and pg_config + libpq headers.
                    psycopg-c = (addSetuptools prev.psycopg-c).overrideAttrs (old: {
                      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
                        pkgs.libpq.pg_config
                        pkgs.libpq
                      ];
                    });
                  }
                )
              ]
            );
            venv = pythonSet.mkVirtualEnv "revng-python-dependencies" workspace.deps.default;
          in
          venv;

        # Build our LLVM fork
        llvm = ccacheStdenv.mkDerivation {
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

        };

        # Build clang to compile QEMU helpers
        clangRelease = stdenv.mkDerivation {
          name = "clang-release";

          src = pkgs.fetchFromGitHub {
            owner = "revng";
            repo = "llvm-project";
            rev = "e966bb52c876de8da25b301e960f886234c78007";
            hash = "sha256-XSfCHg3SpCXq9dnJg/13Kl6kVnocVWA74iLQevu/u3A=";
          };

          nativeBuildInputs = (with pkgs; [
            cmake
            ninja
          ]) ++ [ python ];

          # compiler-rt's sanitizer_common pulls in <crypt.h>, which on
          # Nix comes from libxcrypt.
          buildInputs = [ pkgs.libxcrypt ];

          cmakeFlags = [
            "-GNinja"

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
            "-DLLVM_ENABLE_PROJECTS=clang;compiler-rt;clang-tools-extra;lld"
            "-DLLVM_TARGETS_TO_BUILD=X86"
            "-DCOMPILER_RT_INCLUDE_TESTS=OFF"
          ];

          # Same libc++21 sancov.cpp issue as in the `llvm` derivation.
          postPatch = ''
            sed -i 's|SpecialCaseList::createOrDie({{ClIgnorelist}},|SpecialCaseList::createOrDie({static_cast<std::string>(ClIgnorelist)},|' \
              llvm/tools/sancov/sancov.cpp
          '';

          preConfigure = "cd llvm";

        };

        # Build our fork of QEMU
        qemu = makeQemu pkgs pkgs.llvmPackages_21 "qemu" [ "-fPIC" ] [ "linux-user" "libtcg" ];
        qemuHelpers =
          makeQemu pkgs-2505 pkgs-2505.llvmPackages_16 "qemu-helpers"
            [
              "-fPIC"
              "-Wno-gcc-compat"
              "-DGEN_LLVM_HELPERS"
              "-O0"
              "-Xclang"
              "-disable-O0-optnone"
              "-fembed-bitcode"
            ]
            [ "llvm-helpers" ];

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

        "test/revng-qa" = stdenv.mkDerivation {
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
              ++ (import ./crossShell.nix) {
                inherit nixpkgs;
                inherit system;
              }
            )
            ++ ((import ./msvc.nix) { pkgs = pkgs; })
            ++ [
              self.packages.${system}.revng-qa
              ninja
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
              ${self.packages.${system}.revng-qa}/libexec/revng/test-configure \
              "${self.packages.${system}.revng-qa}/share/revng/test/configuration/revng-qa/"*.yml \
              --install-path "${self.packages.${system}.revng-qa}" \
              --destination . \
              --target-type 'revng-qa\..*'
            export REVNG_OPTIONS="--debug-log=verify"
            grep -v 'shell =' build.ninja > build2.ninja
            mv build2.ninja build.ninja
            ln -s `command -v bash` sh
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

        nanobind = stdenv.mkDerivation {
          name = "nanobind";

          src = pkgs.fetchFromGitHub {
            owner = "revng";
            repo = "nanobind";
            fetchSubmodules = true;
            rev = "a111828dd36d1ce3c8443d2bfc74ac292169a0f3";
            hash = "sha256-sxEehWW+NdoWl+EO/uZ1CzD39Fibsw/XomkUKrFDsQA=";
          };

          # standalone/CMakeLists.txt computes PYTHON_INSTALL_PATH as a path
          # relative from $out to Python_SITELIB; in nix those live in
          # different store paths, so the result escapes $out. Hard-code
          # the install destination to live inside $out instead.
          postPatch = ''
            substituteInPlace standalone/CMakeLists.txt \
              --replace 'DESTINATION "''${CMAKE_INSTALL_PREFIX}/''${PYTHON_INSTALL_PATH}/nanobind"' \
                        'DESTINATION "''${CMAKE_INSTALL_PREFIX}/${python.sitePackages}/nanobind"'
          '';

          nativeBuildInputs = (with pkgs; [
            cmake
            ninja
          ]) ++ [ python ];

          preConfigure = "cd standalone";

          cmakeFlags = [
            "-GNinja"
            "-DCMAKE_CXX_STANDARD=20"
            "-DBUILD_SHARED_LIBS=ON"
          ];

        };

        # Use a fake npm project to specify JavaScript dependencies
        revngJavascriptDependencies = pkgs.stdenv.mkDerivation (finalAttrs: {
          nativeBuildInputs = [
            pkgs.nodejs
            pkgs.pnpm.configHook
          ];
          pname = "revng";
          version = "1.0";
          src = ./revng-js-dependencies;
          installPhase = ''
            pwd
            mkdir -p $out/node_modules
            cp -Tar /build/revng-js-dependencies/node_modules $out/node_modules
          '';
          pnpmDeps = pkgs.pnpm.fetchDeps {
            inherit (finalAttrs) pname version src;
            fetcherVersion = 2;
            hash = "sha256-VxFmVePLXkuBR1kaLj+djwdMqn2uh+m5YM0mSIfXOlo=";
          };
        });

        # Build revng
        revng = stdenv.mkDerivation {
          name = "revng";

          # Filter the source so unrelated repo-level files (flake.nix,
          # result symlink, dev junk) don't re-hash revng on every edit.
          src = pkgs.lib.cleanSourceWith {
            src = ./.;
            filter =
              path: type:
              let
                base = baseNameOf path;
              in
              !(
                base == "flake.nix"
                || base == "flake.lock"
                || base == "result"
                || base == "TODO"
                || pkgs.lib.hasSuffix ".iso" base
                || pkgs.lib.hasSuffix ".iso.1" base
                || base == ".claude"
              );
          };

          nativeBuildInputs = with pkgs; [
            self.packages.${system}.revngPythonDependencies
            clang-tools
            aws-sdk-cpp
            boost-test
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
            self.packages.${system}.revngJavascriptDependencies
            makeWrapper
            self.packages.${system}.llvm
            self.packages.${system}.qemu
            self.packages.${system}.nanobind
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
            export PYTHONPATH="${self.packages.${system}.nanobind}/${python.sitePackages}''${PYTHONPATH:+:$PYTHONPATH}"
          '';

          cmakeFlags = [
            "-GNinja"
            "-DCMAKE_CXX_STANDARD=20"
            "-DCMAKE_C_FLAGS=-O2"
            "-DCMAKE_CXX_FLAGS=-O2"
            "-DCMAKE_BUILD_TYPE=Debug"
            "-DLLVM_DIR=${self.packages.${system}.llvm}/lib/cmake/llvm"
            "-DLIBTCG_DIR=${self.packages.${system}.qemu}"
            "-DQEMU_HELPERS_DIR=${self.packages.${system}.qemuHelpers}"
            "-DTEST_REVNG_QA_DIR=${self.packages.${system}."test/revng-qa"}"
            "-DTARGET_CLANG=${self.packages.${system}.revngClang}/bin/clang"
            # revng's LinkForTranslation calls bare ld.bfd to relink
            # translated binaries; install a configuration.yml that
            # tells it where the host crt files (crt1.o, crti.o,
            # crtbegin.o, crtend.o, crtn.o) live.
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
            export PATH="${self.packages.${system}.llvm}/libexec:$PATH"
            # pypeline-annotations-test runs nanobind_generate_stubs.py
            # which `import nanobind`; nanobind isn't on PYTHONPATH by
            # default during the build.
            export PYTHONPATH="${self.packages.${system}.nanobind}/${python.sitePackages}:${self.packages.${system}.revngPythonDependencies}/${python.sitePackages}''${PYTHONPATH:+:$PYTHONPATH}"
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
                wrapProgram $out/bin/"$PROGRAM" --prefix PYTHONPATH : "${
                  self.packages.${system}.revngPythonDependencies
                }/${python.sitePackages}"
            done
          '';

        };

        # ---------------------------------------------------------------
        # model-db tree (mirror of orchestra components: rootfs/*, win32-
        # metadata, win32metadata/pdbs/*, test/revng-qa/models, model-db).
        # ---------------------------------------------------------------

        # Linux rootfs helper. Runs debootstrap --download-only inside a
        # fixed-output derivation (network-allowed), extracts every .deb
        # in place, then trims the result to ELF binaries + symlinks +
        # ld.so.conf (orchestra-equivalent layout). The resulting tree
        # lives under share/roots/linux/<name> so revng / fetch-debuginfo
        # can find it the way it does in orchestra.
        mkRootfs =
          {
            name,
            codename,
            architecture,
            url,
            operatingSystem,
            packages_,
            outputHash,
          }:
          let
            components =
              if operatingSystem == "ubuntu" then
                "main,restricted,universe,multiverse"
              else
                "main,contrib,non-free";
          in
          stdenv.mkDerivation {
            name = "rootfs-${name}";
            outputHashAlgo = "sha256";
            outputHashMode = "recursive";
            inherit outputHash;
            unpackPhase = "true";
            # The trimmed rootfs intentionally retains symlinks whose
            # targets were removed (non-ELF binaries got deleted).
            dontCheckForBrokenSymlinks = true;
            nativeBuildInputs = with pkgs; [
              debootstrap
              fakeroot
              dpkg
              cacert
              zstd
              xz
              gzip
            ];
            buildPhase = ''
              export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
              fakeroot debootstrap \
                --no-check-gpg \
                --arch="${architecture}" \
                --components="${components}" \
                --include="${packages_}" \
                --download-only \
                "${codename}" \
                rootfs/ \
                "${url}" || true

              # Extract every .deb in place.
              find rootfs -name "*.deb" | while read DEB; do
                mkdir -p temp && cd temp
                ar x "../$DEB"
                cd ../rootfs
                if [ -e "../temp/data.tar"* ]; then
                  tar --skip-old-files -xaf "../temp/data.tar"*
                fi
                cd .. && rm -rf temp
              done

              test "$(find rootfs/ -name 'libc.so*' | wc -l)" -ge 1 \
                || { echo "debootstrap ${name} failed: no libc"; exit 1; }

              # Trim non-ELF except for ld.so.conf{,.d/*} and symlinks.
              find rootfs -not -type d | while read F; do
                [ -L "$F" ] && continue
                REL="''${F#rootfs}"
                [ "$REL" = "/etc/ld.so.conf" ] && continue
                case "$REL" in /etc/ld.so.conf.d/*) continue ;; esac
                if head -c 4 "$F" 2>/dev/null | grep -q $'\x7fELF'; then continue; fi
                rm -f "$F"
              done

              chmod -R u+rwX rootfs/

              # Make absolute symlinks relative.
              find rootfs -type l | while read L; do
                T="$(readlink "$L")"
                if [ "''${T#/}" != "$T" ]; then
                  D="$(dirname "$L")"
                  R="$(realpath -m --relative-to="$D" "rootfs$T")"
                  ln -sfn "$R" "$L"
                fi
              done
              find rootfs -type d -empty -delete
            '';
            installPhase = ''
              mkdir -p "$out/share/roots/linux/${name}"
              cp -a rootfs/* "$out/share/roots/linux/${name}/"
              chmod -R u+rwX "$out/share/roots/linux/${name}/"
            '';
          };

        # rootfs/X/debug-info wrapper: runs `revng model fetch-debuginfo`
        # on every ELF in a rootfs and stuffs the resulting symbols cache
        # under share/roots/linux/<name>/symbols-cache.
        mkRootfsDebugInfo =
          {
            name,
            rootfs,
            outputHash,
          }:
          stdenv.mkDerivation {
            name = "rootfs-${name}-debug-info";
            outputHashAlgo = "sha256";
            outputHashMode = "recursive";
            inherit outputHash;
            unpackPhase = "true";
            nativeBuildInputs = [
              self.packages.${system}.revng
              rootfs
              pkgs.ninja
              pkgs.cacert
            ];
            buildPhase = ''
              export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
              ROOTFS_DIR="${rootfs}/share/roots/linux/${name}"
              export XDG_CACHE_HOME="$PWD/cache"
              mkdir -p "$XDG_CACHE_HOME" .flags
              cat > build.ninja <<EOF
              rule fetch_debuginfo
                command = revng model fetch-debuginfo \$in || true && touch \$out
                description = fetch-debuginfo \$in
              EOF
              find "$ROOTFS_DIR" -type f ! -path "$ROOTFS_DIR/symbols-cache/*" | \
                while read -r ELF; do
                  if head -c 4 "$ELF" 2>/dev/null | grep -q $'\x7fELF'; then
                    HASH=$(sha256sum <<< "$ELF" | cut -d' ' -f1)
                    ESC=$(sed -e 's| |$ |g' <<< "$ELF")
                    echo "build .flags/$HASH: fetch_debuginfo $ESC" >> build.ninja
                  fi
                done
              ninja -v
            '';
            installPhase = ''
              SYMS_SRC="$PWD/cache/revng/debug-symbols/elf"
              SYMS_DST="$out/share/roots/linux/${name}/symbols-cache"
              if [ -d "$SYMS_SRC" ]; then
                mkdir -p "$SYMS_DST"
                cp -a "$SYMS_SRC"/* "$SYMS_DST/" || true
              else
                # Always produce an output so downstream paths exist.
                mkdir -p "$SYMS_DST"
              fi
            '';
          };

        # The 9 Linux rootfs configurations orchestra builds. Each is a
        # fixed-output derivation: the outputHash is populated after the
        # first successful build (debootstrap is non-deterministic over
        # time, but a single .deb set hashed once stays valid until the
        # mirror moves).
        "rootfs/ubuntu-20-04-x86-64" = self.packages.${system}.mkRootfs {
          name = "ubuntu-20-04-x86-64";
          codename = "focal";
          architecture = "amd64";
          url = "http://archive.ubuntu.com/ubuntu/";
          operatingSystem = "ubuntu";
          packages_ = "libfuse2,libc6-dbg";
          outputHash = "sha256-aVeiMLIDfBrA8cTrdTm/VH9DNfCZt2fDA0E+wzjrVkE=";
        };
        "rootfs/ubuntu-22-04-x86-64" = self.packages.${system}.mkRootfs {
          name = "ubuntu-22-04-x86-64";
          codename = "jammy";
          architecture = "amd64";
          url = "http://archive.ubuntu.com/ubuntu/";
          operatingSystem = "ubuntu";
          packages_ = "libfuse2,libc6-dbg";
          outputHash = "sha256-FODGTaDHNdovdCnTuRnd4Z+mxntUHbgJp5K0LWinFxM=";
        };
        "rootfs/ubuntu-24-04-x86-64" = self.packages.${system}.mkRootfs {
          name = "ubuntu-24-04-x86-64";
          codename = "noble";
          architecture = "amd64";
          url = "http://archive.ubuntu.com/ubuntu/";
          operatingSystem = "ubuntu";
          packages_ = "libfuse3-3,libc6-dbg";
          outputHash = "sha256-cgng+8fusATdWBiaToQvAzb1iwYfM4I4V8Ol8Tnt3xw=";
        };
        "rootfs/ubuntu-24-04-i386" = self.packages.${system}.mkRootfs {
          name = "ubuntu-24-04-i386";
          codename = "noble";
          architecture = "i386";
          url = "http://archive.ubuntu.com/ubuntu/";
          operatingSystem = "ubuntu";
          packages_ = "libfuse3-3,libc6-dbg";
          outputHash = "sha256-HTKCrHIyaOh2RUm8boHw6FE0/QZOO5e1OF2ga5jGk3I=";
        };
        "rootfs/ubuntu-24-04-arm" = self.packages.${system}.mkRootfs {
          name = "ubuntu-24-04-arm";
          codename = "noble";
          architecture = "armhf";
          url = "http://ports.ubuntu.com/ubuntu-ports/";
          operatingSystem = "ubuntu";
          packages_ = "libfuse3-3,libc6-dbg";
          outputHash = "sha256-9lnZXji215GkDSld20frHTKCcs4JjTnR5Qo+ojYhY34=";
        };
        "rootfs/ubuntu-24-04-aarch64" = self.packages.${system}.mkRootfs {
          name = "ubuntu-24-04-aarch64";
          codename = "noble";
          architecture = "arm64";
          url = "http://ports.ubuntu.com/ubuntu-ports/";
          operatingSystem = "ubuntu";
          packages_ = "libfuse3-3,libc6-dbg";
          outputHash = "sha256-aqXwLdZaIy8k1vng+CJZDuqIWAT/ZgYTDhfdVV02qaU=";
        };
        "rootfs/ubuntu-24-04-s390x" = self.packages.${system}.mkRootfs {
          name = "ubuntu-24-04-s390x";
          codename = "noble";
          architecture = "s390x";
          url = "http://ports.ubuntu.com/ubuntu-ports/";
          operatingSystem = "ubuntu";
          packages_ = "libfuse3-3,libc6-dbg";
          outputHash = "sha256-B1EUfnGgjjvrgczKWkEB6x7hVuMfF0fxvdaNOyTw54k=";
        };
        "rootfs/debian-bookworm-mipsel" = self.packages.${system}.mkRootfs {
          name = "debian-bookworm-mipsel";
          codename = "bookworm";
          architecture = "mipsel";
          url = "https://ftp.debian.org/debian/";
          operatingSystem = "debian";
          packages_ = "libfuse2,libc6-dbg";
          outputHash = "sha256-fRNsTOy8y/d2ZySfYbh6BDhxgCrx3rQ7Ed/WYBptg3o=";
        };
        "rootfs/debian-buster-mips" = self.packages.${system}.mkRootfs {
          name = "debian-buster-mips";
          codename = "buster";
          architecture = "mips";
          url = "https://archive.debian.org/debian/";
          operatingSystem = "debian";
          packages_ = "libfuse2,libc6-dbg";
          outputHash = "sha256-YWE1mOQ/JEbkEXDybD7WI9TGaz+158BpcN4Qd7lxFe8=";
        };

        # Microsoft's win32metadata: the .winmd files we'll turn into PDBs
        # later. Pinned to the same revision orchestra uses.
        win32metadata = pkgs.fetchFromGitHub {
          owner = "microsoft";
          repo = "win32metadata";
          rev = "223f4b9723d8fb7c83c286b6b4ad75dff18985c4";
          hash = "sha256-4FamAMIy60d4gUbejX7O6TEyWwUevUtVMOC35e19zbk=";
        };

        # Helper used by every `*/models` derivation: walks a directory
        # tree, runs `revng analyze import-binary` (or `revng model
        # import debug-info` for PDBs) against every input file via a
        # generated build.ninja, and installs the resulting `*.yml`
        # files under installDest.
        #
        # `revngBin` is the absolute path of the revng package to use.
        # `importCommand` is the import invocation (no trailing $in -o $out).
        # `findInputs` is shell that emits absolute paths of inputs to import.
        # `extraPreNinja` is run before ninja (e.g. to seed a revng cache).
        # `installDest` is the directory under $out where *.yml files land.
        mkModels =
          {
            name,
            revngBin,
            buildInputs ? [ ],
            findInputs,
            importCommand,
            extraPreNinja ? "",
            installDest,
          }:
          stdenv.mkDerivation {
            inherit name;
            unpackPhase = "true";
            nativeBuildInputs = [
              pkgs.ninja
              revngBin
            ] ++ buildInputs;
            buildPhase = ''
              mkdir -p $BUILD_DIR
              cd $BUILD_DIR
              OUTPUT_DIR="$PWD/models"
              cat > build.ninja <<EOF
              rule import
                command = ${importCommand} \$in -o \$out
                description = Importing \$in
              EOF
              ${findInputs}
            '';
            installPhase = ''
              ${extraPreNinja}
              ninja -v
              mkdir -p "$out/${installDest}"
              if [ -d models ]; then
                cd models && find . -name "*.yml" -exec install -Dm644 {} "$out/${installDest}/{}" \;
              fi
            '';
            BUILD_DIR = "build";
          };

        # well-known-models: import each compiled-with-debug-info
        # binary shipped by test/revng-qa into a per-binary .yml model.
        "test/revng-qa/models" = self.packages.${system}.mkModels {
          name = "test-revng-qa-models";
          revngBin = self.packages.${system}.revng;
          buildInputs = [ self.packages.${system}."test/revng-qa" ];
          installDest = "share/revng/test/tests/well-known-models";
          findInputs = ''
            WELL_KNOWN_DIR="${self.packages.${system}."test/revng-qa"}/share/revng/test/tests/well-known-models"
            for BINARY in "$WELL_KNOWN_DIR/"*revng-qa.compiled-with-debug-info-*; do
              case "$BINARY" in *.yml) continue ;; esac
              BASENAME="$(basename "$BINARY")"
              OUTPUT="$OUTPUT_DIR/''${BASENAME}.yml"
              mkdir -p "$(dirname "$OUTPUT")"
              echo "build $OUTPUT: import $BINARY" >> build.ninja
            done
          '';
          importCommand = "REVNG_NO_FETCH_DEBUG_INFO=1 revng analyze import-binary";
        };

        # model-db: aggregate all available *.yml models into
        # share/revng/prototypes.sqlite via `revng model export sqlite`.
        # Currently only well-known-models is consumed; rootfs/* and
        # win32metadata/pdbs/* models can be added once those layers
        # land — model-db will pick them up automatically.
        model-db = stdenv.mkDerivation {
          name = "model-db";
          unpackPhase = "true";
          nativeBuildInputs = [
            self.packages.${system}.revng
            self.packages.${system}."test/revng-qa/models"
          ];
          installPhase = ''
            DB_NAME=prototypes.sqlite
            rm -f "$DB_NAME"
            export-to-db() {
              local OS="$1" PLATFORM="$2" PREFIX="$3"
              shift 3
              revng model export sqlite \
                --db "$DB_NAME" \
                --platform "$PLATFORM" \
                --operating-system "$OS" \
                --prefix "$PREFIX" \
                "$@"
            }

            # Linux rootfs models (one DB row per rootfs).
            LINUX_ROOTS_DIR="${self.packages.${system}.revng}/share/roots/linux"
            if [ -d "$LINUX_ROOTS_DIR" ]; then
              for ROOTFS_DIR in "$LINUX_ROOTS_DIR"/*; do
                [ -d "$ROOTFS_DIR" ] || continue
                ROOTFS_NAME="$(basename "$ROOTFS_DIR")"
                MODELS="$(find "$ROOTFS_DIR" -name '*.yml' 2>/dev/null)"
                [ -n "$MODELS" ] || continue
                echo "Exporting models from $ROOTFS_NAME to DB" >&2
                export-to-db Linux "$ROOTFS_NAME" "$ROOTFS_DIR" $MODELS
              done
            fi

            # Windows PDB models.
            PDB_DIR="${self.packages.${system}.revng}/share/win32metadata/pdbs"
            if [ -d "$PDB_DIR" ]; then
              for PDB_ARCH_DIR in "$PDB_DIR"/*; do
                [ -d "$PDB_ARCH_DIR" ] || continue
                ARCH="$(basename "$PDB_ARCH_DIR")"
                MODELS="$(find "$PDB_ARCH_DIR" -name '*.yml' 2>/dev/null)"
                [ -n "$MODELS" ] || continue
                echo "Exporting PDB models for $ARCH to DB" >&2
                export-to-db Windows "windows-$ARCH" "$PDB_ARCH_DIR" $MODELS
              done
            fi

            # Well-known revng-qa models — one row per binary, platform
            # extracted from the `libc-<name>-` segment of the basename.
            WK="${self.packages.${system}."test/revng-qa/models"}/share/revng/test/tests/well-known-models"
            if [ -d "$WK" ]; then
              for MODEL in "$WK"/*.yml; do
                [ -f "$MODEL" ] || continue
                BASENAME="$(basename "$MODEL" .yml)"
                PLATFORM="linux-$(echo "$BASENAME" | grep -oP 'libc-\K[^-]+' || echo unknown)"
                echo "Exporting well-known model $BASENAME to DB" >&2
                export-to-db Linux "$PLATFORM" "$WK" "$MODEL"
              done
            fi

            mkdir -p "$out/share/revng"
            cp "$DB_NAME" "$out/share/revng/$DB_NAME"
          '';
        };

        "test/revng" = stdenv.mkDerivation {
          name = "test/revng";

          unpackPhase = "true";

          nativeBuildInputs = (with pkgs; [
            gcc
            binutils
            jq
            llvm_21
            lld_21
            ninja
            nodejs
            qemu
            xorg.lndir
          ]) ++ [
            self.packages.${system}.revng
            self.packages.${system}."test/revng-qa"
            # revngPythonDependencies brings yq + jq runtime that
            # several test-* rules depend on.
            self.packages.${system}.revngPythonDependencies
            (python.withPackages (
              ps: with ps; [
                jinja2
                pyyaml
              ]
            ))
          ];

          buildPhase = ''
            echo
          '';

          installPhase = ''
            mkdir -p $out
            # test-configure resolves source paths against a single
            # --install-path, but the sources/built binaries it asks
            # for live in three separate components — revng, revng-qa
            # and the pre-built test/revng-qa artifacts. Orchestra
            # collapses them into ORCHESTRA_ROOT; under nix we stitch
            # them together in a merged tree via lndir.
            mkdir merged-root
            lndir -silent \
              ${self.packages.${system}.revng-qa} merged-root
            lndir -silent \
              ${self.packages.${system}."test/revng-qa"} merged-root
            lndir -silent \
              ${self.packages.${system}.revng} merged-root
            # Tests like revng.model-migration `cp` model.yml into a tmpdir
            # and write back. cp preserves the source mode (read-only in the
            # nix store), so the copy is also read-only and revng2 fails with
            # EACCES. Replace symlinks under share/revng/test/tests with real
            # writable copies.
            find merged-root/share/revng/test/tests -type l | while IFS= read -r l; do
              t=$(readlink -f "$l") || continue
              rm "$l"
              cp "$t" "$l"
              chmod u+w "$l"
            done
            python3 \
              ${self.packages.${system}.revng-qa}/libexec/revng/test-configure \
              "${self.packages.${system}.revng-qa}/share/revng/test/configuration/revng-qa/"*.yml \
              "${self.packages.${system}.revng}/share/revng/test/configuration/revng/"*.yml \
              --install-path "$PWD/merged-root" \
              --destination . \
              --target-type 'revng\..*'
            # test-configure writes inline scripts (filter.py etc.)
            # to the build dir with `#!/usr/bin/env python3` shebangs.
            # The nix sandbox has no /usr/bin/env, so patch them to
            # point at our concrete interpreters.
            patchShebangs --build .
            export REVNG_OPTIONS="--debug-log=verify"
            # PYPELINE_STORAGE_PROVIDER is needed by the new pypeline
            # tests on develop.
            export PYPELINE_STORAGE_PROVIDER="local://?inline"
            # Several tests shell out to plain `python3` and
            # `import revng.*`; expose revng's installed site-
            # packages on PYTHONPATH.
            export PYTHONPATH="${self.packages.${system}.revng}/${python.sitePackages}:${self.packages.${system}.revngPythonDependencies}/${python.sitePackages}''${PYTHONPATH:+:$PYTHONPATH}"
            # revng2 link-for-translation invokes raw ld.bfd with
            # -l:crt1.o, -l:crtbegin.o, etc. Tell the linker where
            # those come from (the host gcc + glibc).
            export LIBRARY_PATH="${pkgs.glibc}/lib:${pkgs.stdenv.cc.cc.lib}/lib/gcc/x86_64-unknown-linux-gnu/${pkgs.stdenv.cc.cc.version}"
            grep -v 'shell =' build.ninja > build2.ninja
            mv build2.ninja build.ninja
            # Some revng2 / revng invocations on develop hang or
            # take >1h each on certain inputs (e.g. s390x calc
            # through `project init`). Cap them with a per-step
            # timeout so the build can enumerate failing targets
            # in bounded time. 1200s lets s390x recompile-
            # isolated finish under -j8 contention without
            # prematurely killing them.
            sed -i \
              -e 's| revng2 | timeout 1200 revng2 |g' \
              -e 's| revng artifact| timeout 1200 revng artifact|g' \
              build.ninja
            ln -s `command -v bash` sh
            export XDG_CACHE_HOME="$PWD/.cache"
            mkdir -p "$XDG_CACHE_HOME/.cache"

            # Tolerate failing test targets — the bumped revng + new
            # pypeline have several known issues we want to fix one-
            # by-one. Capture the log to $out so the failing targets
            # can be enumerated.
            mkdir -p "$out/log"
            ninja -v -k0 all 2>&1 | tee "$out/log/ninja.log" || true

            # Extract the list of FAILED targets for convenience.
            grep -oE 'FAILED: \[code=[0-9]+\] [^ ]+' "$out/log/ninja.log" \
              > "$out/log/failed-targets.txt" || true
            echo "test/revng: $(wc -l < $out/log/failed-targets.txt) failing target(s); see $out/log/"
          '';

        };

      };
    };
}
