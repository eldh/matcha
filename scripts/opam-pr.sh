#!/usr/bin/env bash
#
# Open the opam-repository pull request for a matcha release.
#
#   scripts/opam-pr.sh 0.2.0 --dry-run   # build the opam file, push nothing
#   scripts/opam-pr.sh 0.2.0             # ... and open the pull request
#
# Run scripts/release.sh first: this script needs the tag to exist on
# GitHub, because it downloads the tag's source archive to checksum it.
#
# What lands in opam-repository is one file:
#   packages/matcha/matcha.<version>/opam
# It is this repository's matcha.opam with a url block appended, naming the
# archive and its checksums. There is no version field - opam takes the
# version from the directory name.
#
# This uses `gh` rather than the `opam publish` plugin, so that it reuses
# the GitHub authentication you already have. RELEASING.md describes the
# `opam publish` alternative and the fully manual fallback.

set -euo pipefail

REPO_SLUG="eldh/matcha"
UPSTREAM="ocaml/opam-repository"
FORK="eldh/opam-repository"
UPSTREAM_BRANCH="master"
CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/matcha-release"

die() {
  printf '\033[31merror:\033[0m %s\n' "$*" >&2
  exit 1
}
step() { printf '\n\033[1m==> %s\033[0m\n' "$*"; }
info() { printf '    %s\n' "$*"; }
skip() { printf '\033[33m    (dry run) would %s\033[0m\n' "$*"; }

usage() {
  sed -n '3,20p' "$0" | sed 's/^# \{0,1\}//'
  exit 1
}

# ---------------------------------------------------------------- arguments

VERSION=""
DRY_RUN=0
for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=1 ;;
    -h | --help) usage ;;
    -*) die "unknown option: $arg" ;;
    *)
      [ -z "$VERSION" ] || die "give exactly one version"
      VERSION="$arg"
      ;;
  esac
done

[ -n "$VERSION" ] || usage
[[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] ||
  die "version must be MAJOR.MINOR.PATCH, got '$VERSION'"

TAG="v$VERSION"
PKG_DIR="packages/matcha/matcha.$VERSION"
BRANCH="release-matcha-$VERSION"
ARCHIVE_URL="https://github.com/$REPO_SLUG/archive/refs/tags/$TAG.tar.gz"

cd "$(git rev-parse --show-toplevel)"
[ -f matcha.opam ] || die "this does not look like the matcha repository"

command -v gh >/dev/null || die "gh is not installed"
gh auth status >/dev/null 2>&1 || die "gh is not authenticated - run: gh auth login"

# ------------------------------------------------------------- the tag first

step "Checking the release"

[ -n "$(git ls-remote --tags origin "refs/tags/$TAG")" ] ||
  die "tag $TAG is not on origin. Run scripts/release.sh $VERSION first."
info "tag $TAG is published"

if gh api "repos/$UPSTREAM/contents/$PKG_DIR" >/dev/null 2>&1; then
  die "$UPSTREAM already has $PKG_DIR - this version is already published"
fi
info "$UPSTREAM has no matcha.$VERSION yet"

# ------------------------------------------------------------- the checksums

step "Downloading and checksumming the archive"

mkdir -p "$CACHE_DIR"
tarball="$CACHE_DIR/matcha-$VERSION.tar.gz"
curl -fsSL -o "$tarball" "$ARCHIVE_URL" ||
  die "could not download $ARCHIVE_URL"

# A truncated or HTML error page would checksum happily, so look inside.
tar -tzf "$tarball" | grep -q '/dune-project$' ||
  die "$tarball does not contain a dune-project - the download is not a
matcha source archive"

hash_of() { openssl dgst "-$2" "$1" | awk '{print $NF}'; }
sha256="$(hash_of "$tarball" sha256)"
sha512="$(hash_of "$tarball" sha512)"

info "src    $ARCHIVE_URL"
info "sha256 $sha256"
info "sha512 $sha512"

# ---------------------------------------------------------- the package file

step "Building the opam file"

opam_file="$(mktemp)"
trap 'rm -f "$opam_file"' EXIT
{
  cat matcha.opam
  cat <<EOF
url {
  src: "$ARCHIVE_URL"
  checksum: [
    "sha256=$sha256"
    "sha512=$sha512"
  ]
}
EOF
} >"$opam_file"

opam lint "$opam_file" || die "opam lint rejected the generated file"

if [ "$DRY_RUN" = 1 ]; then
  step "The file that would be added as $PKG_DIR/opam"
  sed 's/^/    /' "$opam_file"
  skip "clone $UPSTREAM, commit that file on branch $BRANCH,"
  skip "push it to $FORK and open a pull request against $UPSTREAM"
  printf '\n\033[32mDry run finished. Every check passed.\033[0m\n'
  exit 0
fi

# ----------------------------------------------------------------- the clone

step "Preparing a checkout of $UPSTREAM"

# opam-repository has a very long history and is only ever read here at its
# tip, so this is a shallow, single-branch clone, cached between releases.
# It is cloned from UPSTREAM (not the fork) so the branch always sits on
# current master, which means the fork never needs syncing.
clone="$CACHE_DIR/opam-repository"
if [ -d "$clone/.git" ]; then
  info "reusing $clone"
  git -C "$clone" fetch --quiet --depth 1 origin "$UPSTREAM_BRANCH"
  git -C "$clone" checkout --quiet -B "$UPSTREAM_BRANCH" FETCH_HEAD
else
  info "cloning (shallow) into $clone"
  git clone --quiet --depth 1 --single-branch \
    --branch "$UPSTREAM_BRANCH" \
    "https://github.com/$UPSTREAM.git" "$clone"
fi

git -C "$clone" remote get-url fork >/dev/null 2>&1 ||
  git -C "$clone" remote add fork "https://github.com/$FORK.git"

git -C "$clone" checkout --quiet -B "$BRANCH"

step "Adding $PKG_DIR/opam"
mkdir -p "$clone/$PKG_DIR"
cp "$opam_file" "$clone/$PKG_DIR/opam"
git -C "$clone" add "$PKG_DIR/opam"
git -C "$clone" -c user.name="$(git config user.name)" \
  -c user.email="$(git config user.email)" \
  commit --quiet -m "[new release] matcha ($VERSION)"
info "committed"

step "Pushing to $FORK"
git -C "$clone" push --quiet --force-with-lease fork "$BRANCH"
info "pushed $BRANCH"

step "Opening the pull request"
body="$(
  cat <<EOF
matcha $VERSION.

- Release notes: https://github.com/$REPO_SLUG/releases/tag/$TAG
- Changelog: https://github.com/$REPO_SLUG/blob/$TAG/CHANGELOG.md
EOF
)"

gh pr create \
  --repo "$UPSTREAM" \
  --base "$UPSTREAM_BRANCH" \
  --head "${FORK%%/*}:$BRANCH" \
  --title "[new release] matcha ($VERSION)" \
  --body "$body"

printf '\n\033[32mPull request opened.\033[0m\n'
printf 'opam-repository CI will build matcha %s on several compilers.\n' "$VERSION"
printf 'Watch it, and expect a maintainer review before it merges.\n'
