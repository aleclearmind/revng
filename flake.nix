{
  inputs = {
    nixpkgs.url = "git+file:///home/nix/nixpkgs";

    nixpkgs-2505.url = "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-25.05-small.tar.gz";

    # WIP: local pyproject-nix on `feature/windows-pep508`. Adds
    # Windows branches to lib/pep508.nix's PEP 508 environment marker
    # mapping (sys_platform="win32", platform_machine="AMD64", …) so
    # cross-mingw venvs evaluate without "Unsupported platform" throws.
    # Switch back to github: once the patch is upstreamed.
    pyproject-nix = {
      url = "path:/home/nix/pyproject-nix";
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

    # WIP: local revng-qa source — iterating on test-configuration
    # *.yml files. Switch back to a github: URL once upstreamed.
    revng-qa.url = "path:/home/nix/revng-qa";
    revng-qa.flake = false;

    # WIP: local revng-qemu tree on a `feature/windows` branch, where
    # we apply Windows-port commits on top of orchestra's pinned
    # 8035324… revision. Switch back to fetchFromGitHub once the
    # branch is pushed and we're confident in the commits.
    qemu.url = "path:/home/nix/qemu";
    qemu.flake = false;
  };

  outputs =
    inputs@{ self, ... }:
    let
      system = "x86_64-linux";
    in
    {
      packages.${system} = import ./nix { inherit self inputs system; };
    };
}
