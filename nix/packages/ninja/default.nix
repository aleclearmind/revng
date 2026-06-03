{ pkgs }:
# Orchestra's patched ninja: reserves `shell` as a rule binding so
# build.ninja rules can dispatch to a custom wrapper shell. Pinned to
# v1.11.0 — same as orchestra's ninja component — with the
# shell-for-rule.patch applied.
pkgs.ninja.overrideAttrs (oldAttrs: rec {
  version = "1.11.0";
  src = pkgs.fetchFromGitHub {
    owner = "ninja-build";
    repo = "ninja";
    rev = "v${version}";
    hash = "sha256-xZwMdwvg29lauHKk9M318Vz7pXZFhf3kFcyOTBdjmJM=";
  };
  # shell-for-rule.patch is rebased on top of nixpkgs's
  # `0001-spawn-sh-instead-of-bin-sh.patch`: when no `shell = X`
  # rule binding is set we use posix_spawnp("sh") so the sandbox's
  # PATH-resolved sh wins (orchestra's original defaulted to
  # `/bin/sh`, which doesn't exist inside the nix sandbox).
  patches = (oldAttrs.patches or [ ]) ++ [ ./shell-for-rule.patch ];
})
