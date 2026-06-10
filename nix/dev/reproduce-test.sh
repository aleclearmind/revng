#!/usr/bin/env bash
# Reproduce a single test/revng ninja target outside the nix sandbox.
#
# All the dependency wiring (revng, revng-qa, test/revng-qa, the
# merged-root symlinkJoin, the python env, paths, env vars …) is
# already declared in test-revng.nix as `nativeBuildInputs` +
# `preInstall`. We just enter the test/revng devShell so those
# decorations are present, then re-run preInstall in a writable
# workdir before invoking ninja on the chosen target. No need to
# duplicate the dependency list here.
#
# Usage:
#   nix/dev/reproduce-test.sh setup [WORKDIR]
#     Regenerate build.ninja in WORKDIR (default /tmp/revng-test-repro).
#     Idempotent — safe to re-run after a revng / revng-qa / merged-root
#     change so the new store paths land in build.ninja.
#
#   nix/dev/reproduce-test.sh run TARGET [WORKDIR]
#     Run a single ninja target with stderr visible. Auto-runs setup
#     once if build.ninja is missing. The test rules wrap individual
#     binary invocations in `2>/dev/null || true`; we strip both so
#     SIGABRT / SIGILL backtraces are not swallowed.
#
#   nix/dev/reproduce-test.sh shell [WORKDIR]
#     Drop into the test/revng devShell already cd'd into WORKDIR
#     with `eval "$preInstall"` already done — `ninja <target>` works.
set -euo pipefail

cmd=${1:-help}
case "$cmd" in
    run)   TARGET=${2:?"usage: $0 run TARGET [WORKDIR]"}; WORKDIR=${3:-/tmp/revng-test-repro} ;;
    setup|shell) WORKDIR=${2:-/tmp/revng-test-repro} ;;
esac

flake="$(git rev-parse --show-toplevel)"
mkdir -p "$WORKDIR"
wd=$(cd "$WORKDIR" && pwd -P)

# Run `body` inside the test/revng devShell with `cd $wd` already done.
# Don't `source "$stdenv/setup"` — that runs genericBuild and all its
# phases. test-revng.nix's preInstall is written to need nothing from
# the setup hook (patchShebangs is inlined as a small `find … | sed`
# loop), so we can just rely on the env vars + PATH nix develop sets.
devsh() {
    nix develop "$flake#\"test/revng\"" --command bash <<EOF
set -euo pipefail
cd "$wd"
$body
EOF
}

case "$cmd" in
    setup)
        body='
rm -f build.ninja build.ninja.diag
eval "$preInstall"
echo "Setup complete in $(pwd)."
'
        devsh
        ;;
    run)
        body='
if [[ ! -f build.ninja ]]; then
    eval "$preInstall"
fi
if [[ ! -f build.ninja.diag ]] || [[ build.ninja -nt build.ninja.diag ]]; then
    sed -e "s# 2>/dev/null##g" -e "s# || true##g" build.ninja > build.ninja.diag
fi
exec ninja -f build.ninja.diag -v -k0 '"'$TARGET'"'
'
        devsh
        ;;
    shell)
        body='
if [[ ! -f build.ninja ]]; then
    eval "$preInstall"
fi
echo "Entering shell in $(pwd) (Ctrl-D to exit)."
exec "$SHELL"
'
        devsh
        ;;
    *)
        sed -n '/^# Usage:/,/^set -euo/p' "$0" | sed '$d'
        exit 1
        ;;
esac
