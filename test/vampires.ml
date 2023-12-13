(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2023 Miguel Ambrona <mac.ambrona@gmail.com>                 *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Square = Board.Square
module Piece = Board.Piece
module SquareMap = Map.Make (Square)
open Move
open Legality

module Vampires = struct
  let mirror_image pos =
    let mirror_square s =
      let file = Square.file s in
      let rank = Square.rank s in
      Square.of_file_and_rank file (7 - rank)
    in
    let mirror_board =
      SquareMap.fold
        (fun s p mirror_board ->
          let c = Piece.color p in
          let pt = Piece.piece_type p in
          let mirror_p = Piece.make (Color.negate c) pt in
          Board.set_piece (mirror_p, mirror_square s) mirror_board)
        (Position.board pos) Board.empty
    in
    let mirror_cr =
      let cr = pos.castling_rights in
      Position.
        {
          white_short = cr.black_short;
          white_long = cr.black_long;
          black_short = cr.white_short;
          black_long = cr.white_long;
        }
    in
    {
      pos with
      board = mirror_board;
      turn = Color.negate pos.turn;
      castling_rights = mirror_cr;
      en_passant = Option.map mirror_square pos.en_passant;
    }

  (* Precondition: pos must be legal *)
  let is_vampire pos = not (is_legal @@ mirror_image pos)
end

module FenMap = Map.Make (String)

let vampires = ref FenMap.empty
let fetch fen = FenMap.find_opt fen !vampires
let save fen result = vampires := FenMap.add fen result !vampires

let rec find_vampires oc depth pos =
  let pos = Position.{ pos with halfmove_clock = None; fullmove_number = 0 } in
  if depth <= 0 then ()
  else
    let fen = Position.to_fen pos in
    match fetch fen with
    | Some (n, vampire) ->
        if vampire && n < depth then (
          save fen (depth, true);
          if depth > 1 then
            let childs = List.map (apply pos) (legal_moves pos) in
            List.iter (find_vampires oc (depth - 1)) childs)
    | None ->
        if not @@ Vampires.is_vampire pos then (
          save fen (depth, false);
          Printf.fprintf oc "%s|human\n" (Position.to_fen pos))
        else (
          save fen (depth, true);
          Printf.fprintf oc "%s|vampire\n" (Position.to_fen pos);
          if depth > 1 then
            let childs = List.map (apply pos) (legal_moves pos) in
            List.iter (find_vampires oc (depth - 1)) childs)

let () =
  if true then
    (* Just respond to std challenges *)
    let oc_yes = open_out "/tmp/vampires.txt" in
    let oc_no = open_out "/tmp/humans.txt" in
    try
      let cnt = ref 0 in
      while true do
        let fen = input_line stdin in
        cnt := !cnt + 1;
        Format.printf "Analyzing (%d): %s\n" !cnt fen;
        Format.print_flush ();
        let pos = Position.of_fen fen in
        let pos = { pos with halfmove_clock = None } in
        (* let state = Rules.(apply (State.init pos) all_rules) in *)
        (* Debug.print_state state; *)
        if Vampires.is_vampire pos then (
          Printf.fprintf oc_yes "%s\n" fen;
          Format.printf "vampire!\n";
          Format.print_flush ())
        else (
          Printf.fprintf oc_no "%s\n" fen;
          Format.printf "non-vampire\n";
          Format.print_flush ())
      done
    with _ -> ()
  else
    let max_n = int_of_string Sys.argv.(1) in
    let open_goals = ref [] in
    let max_n_analyzed = ref 0 in
    for n = 0 to max_n do
      try
        (* let ic = open_in ("./vampires-depth-" ^ string_of_int n) in *)
        let ic = open_in ("/tmp/vampires2-" ^ string_of_int n) in
        open_goals := [];
        max_n_analyzed := n;
        Format.printf "Fetching depth %d\n" n;
        Format.print_flush ();
        (try
           while true do
             let fen = input_line ic in
             let pos = Position.of_fen fen in
             let pos = { pos with halfmove_clock = None } in
             Format.printf "FEN: %s\n" @@ Position.to_fen pos;
             open_goals := pos :: !open_goals
           done
         with End_of_file -> ());
        close_in ic
      with _ ->
        Format.printf "Analyzing depth %d\n" n;
        Format.print_flush ();
        (* let oc = open_out ("./vampires-depth-" ^ string_of_int n) in *)
        let oc = open_out "/tmp/b2.txt" in
        Format.printf "AAA %d\n" @@ (n - !max_n_analyzed + 1);
        if !open_goals = [] then assert false
        else
          List.iter
            (fun pos -> find_vampires oc (n - !max_n_analyzed + 1) pos)
            !open_goals;

        close_out oc
    done
