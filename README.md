![Sherlock Holmes with City Background](/images/sherlock.png "Sherlock")


[![Tests](https://github.com/miguel-ambrona/sherlock/actions/workflows/ci.yml/badge.svg)](https://github.com/miguel-ambrona/sherlock/actions/workflows/ci.yml)

# Sherlock

> [!IMPORTANT]
> Sherlock (in Ocaml) is no longer under developement. Please, consider
> using the [Rust version of Sherlock](https://github.com/miguel-ambrona/sherlock)
> instead.

A chess library written in OCaml, oriented to creating and solving chess
compositions with especial emphasis on **retrograde analysis**.

This library's name is inspired by the amazing book *"The Chess Mysteries
of Sherlock Holmes"*, a master piece on retrograde analysis by the great
Raymond Smullyan.

## Installation

After cloning the repository:

1. Install a new [opam](https://opam.ocaml.org/) switch by running
`./scripts/install_build_deps.sh`.

2. Compile the tool by with `dune build`.

3. Run the tests with `dune test`.

## Usage

You can run `dune exec retractor/retractor.exe` to start a process that
waits for queries from stdin. A query must be a command followed by a valid
[FEN](https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation).

The following commands are supported:

 - `retract`: retracts all possible moves on the given position.
 This corresponds to all retractions pseudo-legal retractions that lead
 to a well-formed position (e.g. no pawns on the first rank, exactly one
 king of each color, etc). The retracted positions may be illegal.

 - `legal`: determines the legality of the given position. Two outputs
 are possible:
   - *illegal*: meaning that the position is definitely illegal
   (unreachable from the starting position by a sequence of legal moves).
   - *TBD*: meaning that the position is probably legal, or if it is
   illegal, the current logic of Sherlock was not able to determine so.
