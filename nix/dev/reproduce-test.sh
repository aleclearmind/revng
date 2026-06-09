#!/usr/bin/env bash
# Reproduce a failing test/revng target outside the nix sandbox.
#
# Usage:
#   nix/dev/reproduce-test.sh setup [WORKDIR]
#     Build the three input store paths, set up WORKDIR (default:
#     /tmp/revng-test-repro), run test-configure, and emit a build.ninja.
#
#   nix/dev/reproduce-test.sh run TARGET [WORKDIR]
#     Inside WORKDIR, run a single ninja target with verbose stderr
#     visible (the test-revng.nix wrapper redirects to /dev/null).
#
#   nix/dev/reproduce-test.sh shell [WORKDIR]
#     Enter a sub-shell with PATH/PYTHONPATH set so you can run revng2 /
#     ninja / the translated binaries directly.
#
# Quick example:
#   nix/dev/reproduce-test.sh setup
#   nix/dev/reproduce-test.sh run \
#     share/revng/test/tests/runtime/calc-s390x-static-revng.translated-run-061ceeab/
set -euo pipefail

cmd=${1:-help}
WORKDIR=${3:-${2:-/tmp/revng-test-repro}}
# If $2 is a path-like arg for `run`, $3 is workdir; for `setup`/`shell`
# only $2 is the workdir.
case "$cmd" in
    run)  TARGET=${2:?"missing TARGET"}; WORKDIR=${3:-/tmp/revng-test-repro} ;;
    setup|shell) WORKDIR=${2:-/tmp/revng-test-repro} ;;
esac

FLAKE_ROOT=$(git rev-parse --show-toplevel)

resolve_path() {
    # `.#` (not `path:`) keeps the same content-hash semantics nix
    # uses for `nix build .#foo` directly, so we reuse any existing
    # cached store path instead of paying for a rebuild.
    ( cd "$FLAKE_ROOT" && nix build --no-link --print-out-paths ".#$1" 2>/dev/null | tail -1 )
}

setup_env() {
    REVNG=$(resolve_path revng)
    REVNG_QA=$(resolve_path revng-qa)
    TEST_REVNG_QA=$(resolve_path '"test/revng-qa"')
    REVNG_PY_DEPS=$(resolve_path revngPythonDependencies)
    # symlinkJoin'd merged tree matching what test-revng.nix uses.
    # Build it via a tiny nix expression that mirrors the inline let
    # in test-revng.nix (same name + same paths → same store hash).
    MERGED_ROOT=$( cd "$FLAKE_ROOT" && nix build --no-link --print-out-paths --impure --expr \
        'let f = builtins.getFlake (toString ./.);
             p = f.packages.x86_64-linux;
             pkgs = f.inputs.nixpkgs.legacyPackages.x86_64-linux;
         in pkgs.symlinkJoin {
             name = "revng-test-merged-root";
             paths = [
               p.revng-qa p."test/revng-qa" p.revng
               p."rootfs/windows-x86-64" p."rootfs/windows-aarch64"
               p."rootfs/windows-7-x86"  p."rootfs/windows-8-x86-64"
               p."rootfs/windows-8-1-x86-64"
             ];
         }' 2>/dev/null | tail -1 )
    if [[ -z "$REVNG" || -z "$REVNG_QA" || -z "$TEST_REVNG_QA" || -z "$REVNG_PY_DEPS" ]]; then
        echo "Failed to resolve one or more flake outputs. Inputs:" >&2
        echo "  REVNG=$REVNG" >&2
        echo "  REVNG_QA=$REVNG_QA" >&2
        echo "  TEST_REVNG_QA=$TEST_REVNG_QA" >&2
        echo "  REVNG_PY_DEPS=$REVNG_PY_DEPS" >&2
        exit 1
    fi

    # Mirror test-revng.nix nativeBuildInputs PATH-wise via nix shell.
    # Keeping it as env vars here lets us source/exec freely.
    PYTHON_SITELIB="lib/python3.14/site-packages"
    export PATH="$REVNG/bin:$REVNG_QA/bin:$TEST_REVNG_QA/bin:$REVNG_PY_DEPS/bin:$PATH"
    export PYTHONPATH="$REVNG/$PYTHON_SITELIB:$REVNG_PY_DEPS/$PYTHON_SITELIB${PYTHONPATH:+:$PYTHONPATH}"
    export REVNG_OPTIONS="--debug-log=verify"
    export PYPELINE_STORAGE_PROVIDER="local://?inline"
    export XDG_CACHE_HOME="$WORKDIR/.cache"
    mkdir -p "$XDG_CACHE_HOME"
}

case "$cmd" in
    setup)
        mkdir -p "$WORKDIR"
        cd "$WORKDIR"
        setup_env

        # Source paths get printed so the user can copy/paste.
        echo "REVNG=$REVNG"
        echo "REVNG_QA=$REVNG_QA"
        echo "TEST_REVNG_QA=$TEST_REVNG_QA"
        echo "REVNG_PY_DEPS=$REVNG_PY_DEPS"
        echo "WORKDIR=$WORKDIR"

        python3 \
            "$REVNG_QA/libexec/revng/test-configure" \
            "$REVNG_QA/share/revng/test/configuration/revng-qa/"*.yml \
            "$REVNG/share/revng/test/configuration/revng/"*.yml \
            --install-path "$MERGED_ROOT" \
            --destination . \
            --target-type 'revng\..*'

        # Match test-revng.nix postPatch: rewrite #!/usr/bin/env shebangs
        # to the absolute paths visible in this shell.
        patchShebangs --build . 2>/dev/null \
            || find . -maxdepth 2 -type f \( -name "*.py" -o -name "*.sh" \) \
                -exec sed -i '1{s|^#!/usr/bin/env python3|#!'"$(command -v python3)"'|;s|^#!/usr/bin/env bash|#!'"$(command -v bash)"'|}' {} +

        echo
        echo "Setup complete in $WORKDIR."
        echo "Run a target with:"
        echo "  $0 run <ninja-target>"
        ;;
    run)
        cd "$WORKDIR"
        setup_env
        # ninja with -v shows commands; we don't redirect stderr, so
        # abort messages from the translated binaries are visible.
        # The test rules themselves wrap individual binary invocations
        # in `... 2>/dev/null || true`; sed them out for diagnosis.
        if [[ -f build.ninja.diag ]]; then :; else
            sed -e 's| 2>/dev/null||g' -e 's| || true||g' build.ninja > build.ninja.diag
        fi
        exec ninja -f build.ninja.diag -v -k0 "$TARGET"
        ;;
    shell)
        cd "$WORKDIR"
        setup_env
        echo "Entering shell in $WORKDIR (Ctrl-D to exit)."
        exec "$SHELL"
        ;;
    *)
        sed -n '/^# Usage:/,/^set -euo/p' "$0" | sed '$d'
        exit 1
        ;;
esac
