#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

echo "Checking repository is clean after dune build"

if git diff --name-only HEAD --exit-code; then
    echo "Repository clean."
else
    echo "Repository not clean!"
    echo "For .opam files: make changes to dune-project and promote using dune build"
    exit 1
fi
