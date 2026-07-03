{ pkgs, python, inputs, nanobind }:
let
  workspace = inputs.uv2nix.lib.workspace.loadWorkspace {
    workspaceRoot = pkgs.lib.cleanSourceWith {
      name = "revng-python-dependencies";
      src = ./.;
      filter = path: type: baseNameOf path != "default.nix";
    };
  };
  pythonBase = pkgs.callPackage inputs.pyproject-nix.build.packages {
    inherit python;
  };
  overlay = workspace.mkPyprojectOverlay {
    sourcePreference = "wheel";
  };
  pythonSet = pythonBase.overrideScope (
    pkgs.lib.composeManyExtensions [
      inputs.pyproject-build-systems.overlays.wheel
      overlay
      (inputs.uv2nix_hammer_overrides.overrides pkgs)
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
# Drop nanobind into the venv's site-packages so it's discoverable
# alongside the uv2nix-resolved wheels — callers can use this attr
# as a single Python env (no PYTHONPATH gymnastics).
venv.overrideAttrs (old: {
  postInstall = (old.postInstall or "") + ''
    cp -a ${nanobind}/${python.sitePackages}/nanobind \
      $out/${python.sitePackages}/nanobind
  '';
})
