#!/usr/bin/env bash
#
# Cut a matcha release: check, tag, and publish a GitHub release.
#
#   scripts/release.sh 0.2.0 --dry-run   # run every check, change nothing
#   scripts/release.sh 0.2.0             # check, then tag and release
#
# This script never touches the opam repository. Once it succeeds, run
# scripts/opam-pr.sh with the same version to open the opam-repository pull
# request. See RELEASING.md for the whole procedure.
#
# The version lives in exactly one place: the git tag. dune-project carries
# no version field, so there is no file to bump and no release commit to
# make. The only thing that must be committed first is the CHANGELOG entry.

set -euo pipefail

REPO_SLUG="eldh/matcha"
MAIN_BRANCH="main"

die() {
  printf '\033[31merror:\033[0m %s\n' "$*" >&2
  exit 1
}
step() { printf '\n\033[1m==> %s\033[0m\n' "$*"; }
info() { printf '    %s\n' "$*"; }
skip() { printf '\033[33m    (dry run) would %s\033[0m\n' "$*"; }

usage() {
  sed -n '3,14p' "$0" | sed 's/^# \{0,1\}//'
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
cd "$(git rev-parse --show-toplevel)"

# ------------------------------------------------------- preflight: the repo

step "Checking the working tree"

[ -f dune-project ] && [ -f matcha.opam ] ||
  die "this does not look like the matcha repository"

[ -z "$(git status --porcelain)" ] ||
  die "the working tree is not clean - commit or stash first:
$(git status --short)"

branch="$(git rev-parse --abbrev-ref HEAD)"
[ "$branch" = "$MAIN_BRANCH" ] ||
  die "on branch '$branch', expected '$MAIN_BRANCH'"

info "fetching origin"
git fetch --quiet origin --tags

local_head="$(git rev-parse HEAD)"
remote_head="$(git rev-parse "origin/$MAIN_BRANCH")"
[ "$local_head" = "$remote_head" ] ||
  die "$MAIN_BRANCH is not in sync with origin/$MAIN_BRANCH - push or pull first"

git rev-parse -q --verify "refs/tags/$TAG" >/dev/null &&
  die "tag $TAG already exists locally"
[ -z "$(git ls-remote --tags origin "refs/tags/$TAG")" ] ||
  die "tag $TAG already exists on origin"

info "HEAD $(git rev-parse --short HEAD) on $MAIN_BRANCH, clean, $TAG is free"

# ------------------------------------------------- preflight: release inputs

step "Checking the changelog"

grep -qE "^## $VERSION( |$)" CHANGELOG.md ||
  die "CHANGELOG.md has no '## $VERSION' section - write the release notes,
commit them, and run this again"
info "found the '## $VERSION' section"

step "Checking the opam metadata"

eval "$(opam env)" 2>/dev/null || true

dune build matcha.opam
git diff --quiet -- matcha.opam ||
  die "matcha.opam is out of date with dune-project - dune has just
regenerated it. Review and commit the change, then run this again."
info "matcha.opam matches dune-project"

opam lint matcha.opam || die "opam lint failed"

# ------------------------------------------------------ preflight: the build

step "Building and testing"

dune build
dune runtest
info "build clean, suite green"

step "Checking the install manifest"

# matcha is a library. If an examples/*/dune ever regains a (public_name),
# its executable lands here and `opam install matcha` puts a demo binary in
# the user's PATH. test/packaging_tests.re guards this too; the check is
# repeated here because a release is the point of no return.
dune build matcha.install
manifest="_build/default/matcha.install"
if grep -qE '^[[:space:]]*bin:' "$manifest"; then
  die "matcha.install has a bin: section - the package would install
executables. Look for a (public_name ...) in examples/*/dune:
$(grep -A5 -E '^[[:space:]]*bin:' "$manifest")"
fi
# Case-insensitively: dune writes the wrapped library's alias module as
# matcha.cmi, and the casing has moved before.
grep -qi 'matcha/matcha\.cmi' "$manifest" ||
  die "matcha.install does not install the library - something is wrong"
info "installs the library, no executables"

# ------------------------------- preflight: does the published tarball build?

step "Building from a pristine export"

# The strongest check available before publishing: build exactly what the
# tarball will contain, in a directory with no _build and no local switch
# state, with the same command opam will run. This is what catches a file
# that exists on disk but was never committed.
export_dir="$(mktemp -d)"
# INT/TERM/HUP/PIPE as well as EXIT: piping this script into `head` closes
# the pipe early, and a SIGPIPE death would otherwise leave the export - a
# few tens of megabytes - behind.
trap 'rm -rf "$export_dir"' EXIT INT TERM HUP PIPE
git archive --format=tar HEAD | tar -x -C "$export_dir"
info "exported $(git rev-parse --short HEAD) to a temporary directory"
(cd "$export_dir" && dune build -p matcha @install) ||
  die "the exported tree does not build - the tarball would be broken"
info "'dune build -p matcha @install' succeeds on the export"

step "Checking the package build runs no tests"

# opam-repository CI builds with `@install @runtest`, so anything on the
# runtest alias that is not scoped to another package runs inside a
# stranger's install sandbox. matcha's suite opens pseudo-terminals, spawns
# the example binaries and drains them against wall-clock deadlines - fine
# on a developer's machine, and the reason 0.2.0 and 0.3.0 both failed opam
# CI on an emulated riscv64 runner and two experimental macOS ones while
# sixty-seven other jobs passed.
#
# The suite belongs to the matcha-tests package, which is never published.
# This asserts that is still true, on the tree that is about to ship.
ran=$( (cd "$export_dir" && dune build -p matcha @install @runtest 2>&1) |
  grep -cE '\.\.\. (PASS|FAIL)' || true)
[ "$ran" -eq 0 ] ||
  die "a 'dune build -p matcha @install @runtest' on the export executed
$ran tests. They must belong to matcha-tests (or another unpublished
package) so that installing matcha never runs them - see dune-project."
info "'-p matcha @install @runtest' executes no tests"

# --------------------------------------------------------------- the release

notes="$(awk -v v="$VERSION" '
  $0 ~ "^## " v "( |$)" { inside = 1; next }
  inside && /^## / { exit }
  inside { print }
' CHANGELOG.md)"

step "Ready to release matcha $VERSION"
info "tag:     $TAG at $(git rev-parse --short HEAD)"
info "release: https://github.com/$REPO_SLUG/releases/tag/$TAG"
info "tarball: https://github.com/$REPO_SLUG/archive/refs/tags/$TAG.tar.gz"

if [ "$DRY_RUN" = 1 ]; then
  skip "create and push the tag $TAG"
  skip "create the GitHub release with these notes:"
  printf '%s\n' "$notes" | sed 's/^/        /'
  printf '\n\033[32mDry run finished. Every check passed.\033[0m\n'
  exit 0
fi

step "Tagging"
git tag -a "$TAG" -m "matcha $VERSION"
git push origin "$TAG"
info "pushed $TAG"

step "Publishing the GitHub release"
printf '%s\n' "$notes" |
  gh release create "$TAG" \
    --repo "$REPO_SLUG" \
    --title "matcha $VERSION" \
    --notes-file -

printf '\n\033[32mReleased matcha %s.\033[0m\n' "$VERSION"
printf 'Next: scripts/opam-pr.sh %s\n' "$VERSION"
