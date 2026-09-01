# Releasing matcha

Matcha is published to the [opam repository][opam-repo] as the `matcha`
package. A release is three things, in this order:

1. **A git tag** — `v0.2.0`. This is the only place a version number is
   recorded. `dune-project` has no version field, so there is no file to
   bump and no release commit to make.
2. **A GitHub release** — the tag with human-readable notes, taken from
   `CHANGELOG.md`.
3. **A pull request to opam-repository** — one new file,
   `packages/matcha/matcha.0.2.0/opam`, which is this repository's
   `matcha.opam` with a `url` block naming the source archive and its
   checksums. A maintainer reviews it and CI builds the package on several
   compilers before it merges.

Two scripts do all of it.

```
scripts/release.sh 0.2.0     # checks, tag, GitHub release
scripts/opam-pr.sh  0.2.0    # the opam-repository pull request
```

Both take `--dry-run`, which runs every check and changes nothing. **Always
dry-run first.** A tag can be deleted, but an archive that someone has
already downloaded cannot be un-downloaded, and opam checksums the archive
you published.

## Before you start

- `opam`, `dune` and the project switch (`eval $(opam env)` from the repo
  root; the switch lives in `_opam/`).
- [`gh`][gh], authenticated with the `repo` scope (`gh auth status`).
- A fork of `ocaml/opam-repository` on your account. `scripts/opam-pr.sh`
  pushes a branch to it. Create it once with
  `gh repo fork ocaml/opam-repository --clone=false`.
- `curl` and `openssl`, for downloading and checksumming the archive.

## Choosing a version

Semantic versioning, with the usual 0.x caveat: **while the major version is
0, a minor bump is allowed to break the API**, and a patch bump is not.

The public API is exactly what `lib/Matcha.rei` declares. If a symbol
leaves that file, or changes type, that is a breaking change even when
nothing in this repository notices — `examples/` and `test/` are compiled
against the implementation, not against the interface.

## Step 1 — write the changelog

`CHANGELOG.md` needs a `## <version>` section before anything else works;
`scripts/release.sh` refuses to run without one. Write it, commit it, push
it. The section becomes the GitHub release notes verbatim.

## Step 2 — release

```
scripts/release.sh 0.2.0 --dry-run
scripts/release.sh 0.2.0
```

The script refuses to continue unless all of the following hold. Each one
has cost a real release somewhere, which is why they are checks and not
advice:

| Check | Why |
|---|---|
| Working tree clean, on `main`, in sync with `origin/main` | The tag must name a commit other people can fetch. |
| `v<version>` does not exist locally or on the remote | **Never move a published tag.** opam has checksummed the archive it generates. |
| `CHANGELOG.md` has a `## <version>` section | The release notes come from it. |
| `matcha.opam` matches what dune generates from `dune-project` | The checked-in file is what gets copied into opam-repository. A stale one publishes wrong metadata. |
| `opam lint matcha.opam` passes | Catches malformed metadata before a reviewer does. |
| `dune build` and `dune runtest` | The obvious one. |
| `matcha.install` has no `bin:` section | Matcha is a library. Until 0.2.0 the examples carried `(public_name matcha-example-*)`, so installing the package put 15 demo binaries in the user's PATH. `test/packaging_tests.re` guards this in the normal test run as well. |
| `dune build -p matcha @install` succeeds on a pristine `git archive` export | The strongest check available: it builds exactly what the tarball will contain, with the command opam will run, in a directory with no `_build`. This is what catches a file that exists on your disk but was never committed. |

Then it tags, pushes the tag, and creates the GitHub release.

## Step 3 — the opam pull request

```
scripts/opam-pr.sh 0.2.0 --dry-run
scripts/opam-pr.sh 0.2.0
```

It downloads `https://github.com/eldh/matcha/archive/refs/tags/v0.2.0.tar.gz`,
checks that the download really is a source archive rather than an error
page, computes its sha256 and sha512, appends the `url` block to
`matcha.opam`, lints the result, and commits it to a shallow clone of
opam-repository on a `release-matcha-0.2.0` branch. That branch is pushed to
your fork and a pull request titled `[new release] matcha (0.2.0)` is opened
against `master`.

The clone is shallow, single-branch, and cached under
`~/.cache/matcha-release/`. It is cloned from `ocaml/opam-repository` rather
than from your fork, so the branch always sits on current `master` and the
fork never needs syncing.

After that, opam-repository CI builds the package. Expect a review; the
queue is usually hours to a couple of days.

### If CI fails or a reviewer asks for a change

Edit the opam file on the branch in the cached clone and push again — the
pull request updates itself:

```
cd ~/.cache/matcha-release/opam-repository
$EDITOR packages/matcha/matcha.0.2.0/opam
git commit -am "matcha 0.2.0: address review"
git push fork release-matcha-0.2.0
```

**Do not re-tag.** If the source itself is wrong, the fix is a new patch
version, not a moved tag. opam-repository treats a published archive as
immutable, and moving a tag changes the archive under a checksum that
people have already recorded.

## Fields dune does not generate

`matcha.opam` is generated from `dune-project`, so **edit `dune-project`,
never `matcha.opam`**. Two fields have no `dune-project` equivalent and live
in `matcha.opam.template`, which dune appends to the generated file:

- `available: os-family != "windows"` — the terminal layer needs `termios`
  and `ioctl`.
- `x-maintenance-intent: ["(latest)"]` — tells opam-repository that only the
  newest release is maintained.

## Alternatives to the scripts

**The `opam publish` plugin** does steps 2 and 3 in one command and is the
route the opam documentation describes:

```
opam install opam-publish
opam publish --tag=v0.2.0 https://github.com/eldh/matcha/archive/refs/tags/v0.2.0.tar.gz .
```

It works, but it asks for a GitHub token of its own and stores it under
`~/.opam/plugins/opam-publish/`, rather than reusing the `gh`
authentication already on the machine. That is the only reason the scripts
here exist.

**Fully by hand**, if both are unavailable: fork opam-repository, add
`packages/matcha/matcha.<version>/opam` containing `matcha.opam` plus a
`url` block, and open a pull request titled
`[new release] matcha (<version>)`. The `url` block:

```
url {
  src: "https://github.com/eldh/matcha/archive/refs/tags/v0.2.0.tar.gz"
  checksum: [
    "sha256=..."
    "sha512=..."
  ]
}
```

Checksums come from `openssl dgst -sha256 <file>` and `-sha512`.

## Checking the result

Once the pull request merges, the package appears within the hour:

```
opam update
opam show matcha
opam install matcha
```

A useful last check is to build something against the published package
rather than against a local pin, which is what proves the tarball really
contains everything a consumer needs.

[opam-repo]: https://github.com/ocaml/opam-repository
[gh]: https://cli.github.com/
