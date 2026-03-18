#!/usr/bin/env bash
# Upload a package version to Hackage as a candidate or a full release.
#
# Usage:
#   ./upload-hackage.sh <version> [--release]
#
# Examples:
#   ./upload-hackage.sh 0.1.0.0           # upload as candidate
#   ./upload-hackage.sh 0.1.0.0 --release  # publish the release

set -euo pipefail

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------

VERSION=""
RELEASE=false

while [[ $# -gt 0 ]]; do
  case $1 in
    --release)
      RELEASE=true
      shift
      ;;
    -*)
      echo "❌ Unknown option: $1" >&2
      echo "Usage: $0 <version> [--release]" >&2
      exit 1
      ;;
    *)
      if [[ -z "$VERSION" ]]; then
        VERSION="$1"
      else
        echo "❌ Unexpected argument: $1" >&2
        echo "Usage: $0 <version> [--release]" >&2
        exit 1
      fi
      shift
      ;;
  esac
done

if [[ -z "$VERSION" ]]; then
  echo "❌ Error: version is required." >&2
  echo "Usage: $0 <version> [--release]" >&2
  exit 1
fi

if $RELEASE; then
  echo "🚀 Publishing release monad-rail-${VERSION} to Hackage"
else
  echo "🚀 Uploading monad-rail-${VERSION} as a Hackage candidate"
fi

# ---------------------------------------------------------------------------
# Credentials
# ---------------------------------------------------------------------------

read -rp "👤 Hackage username: " HACKAGE_USER
read -rsp "🔑 Hackage password: " HACKAGE_PASS
echo

# ---------------------------------------------------------------------------
# Source distribution
# ---------------------------------------------------------------------------

echo ""
echo "📦 Building source distribution..."
cabal sdist

SDIST="dist-newstyle/sdist/monad-rail-${VERSION}.tar.gz"

if [[ ! -f "$SDIST" ]]; then
  echo "❌ Error: source distribution not found at $SDIST" >&2
  exit 1
fi

echo "⬆️  Uploading package..."
if $RELEASE; then
  cabal upload --publish -u "$HACKAGE_USER" -p "$HACKAGE_PASS" "$SDIST"
else
  cabal upload -u "$HACKAGE_USER" -p "$HACKAGE_PASS" "$SDIST"
fi

# ---------------------------------------------------------------------------
# Documentation
# ---------------------------------------------------------------------------

echo ""
echo "📖 Building Haddock documentation..."
cabal haddock --for-hackage --enable-documentation

DOCS="dist-newstyle/monad-rail-${VERSION}-docs.tar.gz"

if [[ ! -f "$DOCS" ]]; then
  echo "❌ Error: documentation archive not found at $DOCS" >&2
  exit 1
fi

echo "⬆️  Uploading documentation..."
if $RELEASE; then
  cabal upload --documentation --publish -u "$HACKAGE_USER" -p "$HACKAGE_PASS" "$DOCS"
else
  cabal upload --documentation -u "$HACKAGE_USER" -p "$HACKAGE_PASS" "$DOCS"
fi

echo ""
if $RELEASE; then
  echo "✅ Done! monad-rail-${VERSION} is now published on Hackage."
  echo "🔗 https://hackage.haskell.org/package/monad-rail-${VERSION}"
else
  echo "✅ Done! monad-rail-${VERSION} candidate is available at:"
  echo "🔗 https://hackage.haskell.org/package/monad-rail-${VERSION}/candidate"
fi
