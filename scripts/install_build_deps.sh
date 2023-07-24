#! /bin/sh

set -e

ocaml_version=4.14.0
opam_version=2.1

if [ ! "$opam_version" = "$(opam --version | sed -e 's/\([0-9].[0-9]\).[0-9]/\1/')" ]; then echo "Need opam 2.1 for OCaml5"; exit 1; fi

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

build_system_hash_file="$src_dir/.build_system_hash"

compute_build_system_hash () {
    cat $src_dir/dune-project $src_dir/*.opam $src_dir/.gitlab-ci.yml $0 | sha256sum | cut -d' ' -f1
}

# all the opam packages and this script, which contains the versions,
# are hashed and stored in the build_system_hash_file.
# If any of these files changes the CI cache is deleted.

if [ ! "$(compute_build_system_hash)" = "$(cat $build_system_hash_file)" ]
then
    echo 'Inconsistent hash in .build_system_hash'
    if [ "$1" = '--update' ]
    then
        compute_build_system_hash > $build_system_hash_file
        env_changes='true'
        echo 'updated hash. Nuke _opam?'
        read UNUSED
    else exit 1
    fi
fi

if [ ! -d "$src_dir/_opam" ] ; then first_run="true" ; echo "first run" ; fi

if [ "${CI_COMMIT_BRANCH#*opam}" != "$CI_COMMIT_BRANCH" ] ; then opam_branch="true" ; echo "opam branch" ; fi

if [ $first_run ] || [ $env_changes ] || [ $CLEAN_CACHE ] || [ $opam_branch ]; then

    echo "Nuking _opam"

    rm -rf "$src_dir/_opam" "$src_dir/_build"
    export OPAMYES=${OPAMYES:=true}
    export OPAMCONFIRMLEVEL=${OPAMCONFIRMLEVEL:=unsafe-yes}
    opam repository set-url default https://opam.ocaml.org
    opam update
    opam switch create "$src_dir" $ocaml_version --no-install
    opam install ocamlformat.0.25.1
    opam install . --deps-only --with-test

    echo 'You may want to: opam install odoc merlin'

fi
eval $(opam env --shell=sh)
