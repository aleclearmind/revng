{ inputs, system }:
let
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
  pkgs = import inputs.nixpkgs {
    inherit system;
    # overlays = [ ccacheOverlay ];
  };
  pkgs-2505 = import inputs.nixpkgs-2505 {
    inherit system;
    # overlays = [ ccacheOverlay ];
  };

  # Pin Python to 3.14.x.
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
in
{
  inherit pkgs pkgs-2505 python stdenv ccacheStdenv;
}
