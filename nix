#!/usr/bin/env bash

# Self-contained nix wrapper for the revng repo. On first run, downloads
# a pinned nix-portable, writes an isolated nix.conf configured with the
# rev.ng public and private HTTP binary caches, prompts for a GitLab
# token to authenticate to the private cache, and sanity-checks both
# endpoints. Subsequent runs just exec nix inside nix-portable.
#
# State (nix-portable binary, isolated store, config, netrc) lives either
# in a shared XDG cache directory (default: $XDG_CACHE_HOME/revng/nix,
# i.e. ~/.cache/revng/nix), so multiple checkouts of the repo share the
# same store, or in ./.nix next to this script, local to the checkout.
# On first run the script asks which one to use. Delete the picked
# directory to reset.
#
# Example:
#   ./nix build .#revng -j0     # substitute the closure, no local build

set -euo pipefail

# Logging helper: writes to stderr, so stdout stays clean for whatever
# nix-portable produces. Accepts either arguments or stdin (for multi-
# line heredocs).
log() {
    if [ "$#" -eq 0 ]; then
        cat >&2
    else
        printf '%s\n' "$*" >&2
    fi
}

# Pinned nix-portable release.
NIX_PORTABLE_VERSION="${NIX_PORTABLE_VERSION:-v012}"

# x86_64 / aarch64.
NIX_PORTABLE_ARCHITECTURE="$(uname -m)"

NIX_PORTABLE_URL="https://github.com/DavHau/nix-portable/releases/download/${NIX_PORTABLE_VERSION}/nix-portable-${NIX_PORTABLE_ARCHITECTURE}"

# Candidate state directories.
SCRIPT_DIRECTORY="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
XDG_CACHE_ROOT="${XDG_CACHE_HOME:-$HOME/.cache}"
CACHE_DIRECTORY="$XDG_CACHE_ROOT/revng/nix"
LOCAL_DIRECTORY="$SCRIPT_DIRECTORY/.nix"

# The extracted nix-portable binary lives at this path in each candidate.
# If we find it in either, that directory wins and there is no prompt.
CACHE_BINARY="$CACHE_DIRECTORY/nix-portable/nix"
LOCAL_BINARY="$LOCAL_DIRECTORY/nix-portable/nix"

if   [ -x "$CACHE_BINARY" ]; then
    NIX_DIRECTORY="$CACHE_DIRECTORY"
elif [ -x "$LOCAL_BINARY" ]; then
    NIX_DIRECTORY="$LOCAL_DIRECTORY"
else
    log <<EOF
No existing nix-portable install found. Where should it live?

  1) $CACHE_DIRECTORY
     Shared across all revng checkouts (recommended if you have more
     than one checkout — you download nix-portable and populate the
     store once).

  2) $LOCAL_DIRECTORY
     Scoped to this checkout only.

EOF
    read -r -p "Choose [1/2] (default 1): " ANSWER
    case "${ANSWER:-1}" in
        1|"") NIX_DIRECTORY="$CACHE_DIRECTORY" ;;
        2)    NIX_DIRECTORY="$LOCAL_DIRECTORY" ;;
        *)    log "Invalid choice, aborting."; exit 1 ;;
    esac
fi

NIX_PORTABLE_BINARY="$NIX_DIRECTORY/nix-portable/nix"
CONFIG_FILE="$NIX_DIRECTORY/nix.conf"
NETRC_FILE="$NIX_DIRECTORY/netrc"

# Written only after a fully-successful setup. If missing, we redo the
# token prompt, config generation, and sanity check (the nix-portable
# binary is cached so we don't re-download it).
SETUP_COMPLETE_MARKER="$NIX_DIRECTORY/.setup-complete"

# nix-portable stores its unpacked runtime and store under $NP_LOCATION.
export NP_LOCATION="$NIX_DIRECTORY"

# rev.ng caches.
GATING_PROJECT_URL="https://rev.ng/gitlab/revng-private/binary-archives"

# GitLab's access-tokens form reads name / scopes[] / access_level from
# the query string, so the URL below prefills a Reporter-role,
# read_repository token named "nix-binary-cache" — the user just clicks
# Create.
TOKEN_URL="${GATING_PROJECT_URL}/-/settings/access_tokens?name=nix-binary-cache&scopes[]=read_repository&access_level=20"

PUBLIC_CACHE_URL="https://rev.ng/nix-binary-cache/public/"
PRIVATE_CACHE_URL="https://rev.ng/nix-binary-cache/private/"
REVNG_PUBLIC_KEY="revng-cache:Wqy0YTHRGuDijpuHK+3uhP54idwTYbjXfxVqnsfusGU="
NIXOS_PUBLIC_KEY="cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

if [ ! -f "$SETUP_COMPLETE_MARKER" ]; then
    mkdir -p "$(dirname "$NIX_PORTABLE_BINARY")"

    # Skip the download if a previous run made it this far and then
    # failed at the sanity check.
    if [ ! -x "$NIX_PORTABLE_BINARY" ]; then
        log "Downloading pinned nix-portable ${NIX_PORTABLE_VERSION} (${NIX_PORTABLE_ARCHITECTURE}) into ${NIX_DIRECTORY}"
        curl -fsSL "$NIX_PORTABLE_URL" -o "$NIX_PORTABLE_BINARY.tmp"
        chmod +x "$NIX_PORTABLE_BINARY.tmp"
        mv "$NIX_PORTABLE_BINARY.tmp" "$NIX_PORTABLE_BINARY"
    fi

    # Token / netrc bootstrap.
    #
    # Nix's HTTP binary-cache client authenticates via netrc, i.e. HTTP
    # Basic. GitLab rejects Basic on its REST API but accepts a token as
    # the Basic password on git-over-HTTPS, which is what the private-
    # cache nginx auth_request forwards to. The token goes in the
    # password field; any non-empty login works.
    #
    # The token is OPTIONAL: skipping it configures the public cache
    # only, which is enough if the closures you build don't reference
    # any `fetchPrivateUrl`-produced paths (Windows SDKs, non-
    # redistributable tarballs, and so on).
    log <<EOF

The private cache is gated on read access to
  ${GATING_PROJECT_URL}

Create a project access token (form is prefilled: scope read_repository,
role Reporter) at:

  ${TOKEN_URL}

Leave the prompt empty to configure the public cache only — that is
enough unless you build something that pulls a private FOD (e.g. a
Windows SDK).

EOF
    read -r -s -p "Paste the token (input hidden, or ENTER to skip): " TOKEN
    log ""

    # nix.conf, plus optional netrc: two shapes depending on whether the
    # private cache is wired in.
    if [ -n "$TOKEN" ]; then
        ( umask 077
          cat > "$NETRC_FILE" <<EOF
machine rev.ng
  login nix-cache
  password $TOKEN
EOF
        )
        chmod 600 "$NETRC_FILE"
        cat > "$CONFIG_FILE" <<EOF
experimental-features = nix-command flakes
substituters = ${PUBLIC_CACHE_URL} ${PRIVATE_CACHE_URL} https://cache.nixos.org/
trusted-public-keys = ${REVNG_PUBLIC_KEY} ${NIXOS_PUBLIC_KEY}
netrc-file = ${NETRC_FILE}
EOF
    else
        rm -f "$NETRC_FILE"
        cat > "$CONFIG_FILE" <<EOF
experimental-features = nix-command flakes
substituters = ${PUBLIC_CACHE_URL} https://cache.nixos.org/
trusted-public-keys = ${REVNG_PUBLIC_KEY} ${NIXOS_PUBLIC_KEY}
EOF
    fi

    # Sanity check. Always verify the public cache. Only verify the
    # private cache if a token was supplied. Failures come with clear
    # HTTP codes because we do this in curl instead of through nix.
    log ""
    log "Sanity check: nix-cache-info on the configured cache(s)."
    PUBLIC_CODE=$(curl -sS -o /dev/null -w "%{http_code}" -m 15 "${PUBLIC_CACHE_URL}nix-cache-info" || echo error)
    log "    public   -> $PUBLIC_CODE"
    CHECK_FAILED=0
    [ "$PUBLIC_CODE" != 200 ] && CHECK_FAILED=1

    if [ -n "$TOKEN" ]; then
        PRIVATE_CODE=$(curl -sS -o /dev/null -w "%{http_code}" -m 15 -u "nix-cache:$TOKEN" "${PRIVATE_CACHE_URL}nix-cache-info" || echo error)
        log "    private  -> $PRIVATE_CODE"
        [ "$PRIVATE_CODE" != 200 ] && CHECK_FAILED=1
    else
        log "    private  -> skipped (no token)"
    fi

    if [ "$CHECK_FAILED" != 0 ]; then
        log "Sanity check failed. Rerun this script to retry."
        if [ -n "${PRIVATE_CODE:-}" ] && { [ "$PRIVATE_CODE" = 401 ] || [ "$PRIVATE_CODE" = 403 ]; }; then
            log "Private returned $PRIVATE_CODE: check that the token has read_repository"
            log "access on ${GATING_PROJECT_URL} (Reporter role or higher)."
        fi

        # So the next run re-prompts.
        rm -f "$NETRC_FILE" "$CONFIG_FILE"
        exit 1
    fi

    touch "$SETUP_COMPLETE_MARKER"
    log "Setup complete."
    log ""
fi

# NIX_USER_CONF_FILES fully replaces ~/.config/nix/nix.conf, so nothing on
# the host can leak into this env. The nix-portable binary IS nix (a
# self-contained bundle), so we forward argv directly — "./nix build ."
# runs "nix build ." inside the nix-portable environment.
export NIX_USER_CONF_FILES="$CONFIG_FILE"
exec "$NIX_PORTABLE_BINARY" "$@"
