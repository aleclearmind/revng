{ pkgs, inputs }:
# Cross-build of the uv2nix-resolved Python venv targeting our
# windows/python (3.14.6 with MSYS2 patches).
#
# Mirrors `revng-python-dependencies/default.nix` (Linux variant)
# but `callPackage`s pyproject-nix's build packages through
# `pkgsCross.mingwW64` so `mkPythonSet` flips into its cross-aware
# codepath (separate build-host/host-host python scopes, shebang-
# rewriting hook, etc.).
#
# Two prereqs the Linux variant doesn't need:
#  - `windows/python` uses a recursive `self` override (see
#    `python-windows.nix`) so passthru-captured `withPackages`
#    closes over the patched 3.14.6 derivation, not the original
#    broken-marked 3.14.0.
#  - The local fork of `pyproject-nix` (`flake.nix:inputs.pyproject-
#    nix`) adds the Windows branches to `lib/pep508.nix`'s PEP 508
#    environment marker mapping so pkgsCross.mingwW64 doesn't throw
#    "Unsupported platform" on `sys_platform` / `platform_machine`.
let
  lib = pkgs.lib;
  windowsPython = (import ./python-windows.nix) { inherit pkgs; };
  nanobindWindows = (import ./nanobind-windows.nix) { inherit pkgs; };
  windowsCross = pkgs.pkgsCross.mingwW64;

  workspace = inputs.uv2nix.lib.workspace.loadWorkspace {
    workspaceRoot = pkgs.lib.cleanSourceWith {
      name = "revng-python-dependencies";
      src = ./revng-python-dependencies;
      filter = path: type: baseNameOf path != "default.nix";
    };
  };

  pythonBase = windowsCross.callPackage inputs.pyproject-nix.build.packages {
    python = windowsPython;
  };

  overlay = workspace.mkPyprojectOverlay {
    sourcePreference = "wheel";
  };

  pythonSet = pythonBase.overrideScope (
    lib.composeManyExtensions [
      inputs.pyproject-build-systems.overlays.wheel
      overlay
      (inputs.uv2nix_hammer_overrides.overrides pkgs)
      (
        final: prev:
        let
          addSetuptools = drv: drv.overrideAttrs (old: {
            nativeBuildInputs =
              (old.nativeBuildInputs or [ ]) ++ final.resolveBuildSystem { setuptools = [ ]; };
          });
          # Stub a package: skip build, install an empty `${moduleName}`
          # so `from ${moduleName} import …` produces an AttributeError
          # at runtime (loudly, not silently).
          stubPkg = drv: moduleName: drv.overrideAttrs (_old: {
            dontUnpack = true;
            buildPhase = ":";
            installPhase = ''
              mkdir -p $out/${windowsPython.sitePackages}/${moduleName}
              touch $out/${windowsPython.sitePackages}/${moduleName}/__init__.py
            '';
            doCheck = false;
            dontUsePyprojectBuild = true;
            dontUsePyprojectInstall = true;
          });
        in
        {
          grandiso = addSetuptools prev.grandiso;
          python-idb = addSetuptools prev.python-idb;
          llvmcpy = addSetuptools prev.llvmcpy;
          # uvloop bundles libuv and runs `./configure` to detect cross
          # — exit 77 on Windows. The library is POSIX-only upstream
          # ("uvloop does not support Windows"). Stub it; consumers
          # use asyncio's default IOCP event loop on Windows.
          uvloop = stubPkg prev.uvloop "uvloop";

          # psycopg + the C/pool extras: revng disables Postgres-
          # backed pypeline storage on Windows, so we stub the entire
          # psycopg family to empty `__init__.py` modules. Any
          # accidental `import psycopg` will fail at runtime, which
          # is the desired signal (vs silently using a broken
          # cross-built extension).
          psycopg = stubPkg prev.psycopg "psycopg";
          psycopg-c = stubPkg prev.psycopg-c "psycopg_c";
          psycopg-pool = stubPkg prev.psycopg-pool "psycopg_pool";
        }
      )
    ]
  );

  venv = pythonSet.mkVirtualEnv "revng-python-dependencies-windows" workspace.deps.default;
in
venv.overrideAttrs (old: {
  postInstall = (old.postInstall or "") + ''
    cp -a ${nanobindWindows}/${windowsPython.sitePackages}/nanobind \
      $out/${windowsPython.sitePackages}/nanobind
  '';
})
