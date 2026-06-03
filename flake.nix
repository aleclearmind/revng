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
    inputs@{ self, ... }:
    let
      system = "x86_64-linux";
    in
    {
      packages.${system} = import ./nix { inherit self inputs system; };
    };
}
