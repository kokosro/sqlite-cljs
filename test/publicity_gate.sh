#!/usr/bin/env bash
# The publicity gate: src/ and README.md are PUBLIC documentation. They must
# carry no development-process narrative — no plan/workspace file references,
# no internal machine paths, no user-decision provenance, no phase-landing
# stories, no internal evidence tags, no dated session stamps.
#
# Runs FIRST in run-tests.sh (before the suite compiles). Zero network.
# Exit status: 0 when clean; on a hit it prints file:line and fails.
set -uo pipefail
cd "$(dirname "$0")/.."

status=0

# One grep per banned pattern. Each pattern lists what it catches; a comment
# beside it names what is ALLOWED to keep it from flagging (the allowlist
# lives here, not in the source: adding an exception means editing this gate,
# which is where a reviewer sees it).
check () {
  local name="$1" pattern="$2" paths="$3"
  local hits
  hits=$(grep -rnE "$pattern" $paths 2>/dev/null || true)
  if [ -n "$hits" ]; then
    echo "PUBLICITY: $name"
    echo "$hits"
    status=1
  fi
}

check "plan/workspace file references" \
      'plan:|house-rules|workspace\.org|conductor|orchestrator|phase-[0-9a-z-]*-notes' \
      "src README.md"

check "internal machine paths" \
      '/home/|/etc/[a-z]|~/\.(secrets|config|local)' \
      "src README.md"

# A bare ~ in prose is a path reference too, EXCEPT where the code expands it
# (expand-home documents its own argument). Allowed: env-var NAMES, key-file
# NAMES without a home prefix.
check "home-relative path references" \
      '~/[a-zA-Z]' \
      "src README.md"

check "user-decision provenance" \
      '[Uu]ser (directive|ruling|decision|answer|commission)|user-locked|\[USER\]|confirmed by user|user ruling' \
      "src README.md"

check "phase-landing stories" \
      '[Pp]hase [0-9]+ (landed|complete[sd]?)|landed (at|in) |as-built|built (at|in) commit|[0-9a-f]{7,40}\)|commit [0-9a-f]{7,40}' \
      "src README.md"

check "internal evidence tags" \
      '\[(SRC|RPT|INV|CALC|MEM|LIVE|DOC|USER)\]' \
      "src README.md"

check "dated session stamps" \
      '20[0-9]{2}-[0-9]{2}-[0-9]{2}' \
      "src README.md"

# TODO markers addressed to the development process.
check "process TODOs" \
      '(TODO|FIXME|XXX):' \
      "src README.md"

if [ "$status" -ne 0 ]; then
  echo "publicity gate: FAILED — rewrite the lines above to state facts directly"
  exit 1
fi
echo "publicity gate: clean"
