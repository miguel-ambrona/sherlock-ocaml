#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

cleanup () {
    set +e
    if [ -f LOG ]; then
        echo "failed."
        echo
        cat LOG
        rm LOG
        echo
        exit 1
    fi
}
trap cleanup EXIT INT

silent () {
    "$@" > LOG 2>&1
    rm LOG
}

export OPAMYES=${OPAMYES:=true}
export OPAMCONFIRMLEVEL=${OPAMCONFIRMLEVEL:=unsafe-yes}

echo
echo "## Unpinning packages..."

opams=$(find . -name \*.opam -print | grep -Ev '_build|_opam')
packages=
for opam in $opams; do
    dir=$(dirname "$opam")
    file=$(basename "$opam")
    package=${file%.opam}
    packages="$packages $package"
    opam lint $opam
    opam pin remove --no-action "$package" > /dev/null 2>&1
done

echo
echo "## Pinning packages..."

for package in $packages; do
    opam pin add --no-action "$package" "$dir" > /dev/null 2>&1
done

#shellcheck disable=SC2086
packages=$(opam list --short --sort --pinned $packages)

export packages="$packages"
echo
echo "## Pinned packages:"
echo
echo "$packages" | sed 's/^/ /'
echo

for package in $packages; do
    echo -n "Installing $package..."
    silent opam install $package
    echo " OK."

    echo -n "Removing $package..."
    silent opam remove -a $package
    echo " OK."
done
