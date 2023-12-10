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

  let quick_vampire_test pos =
    let board = Position.board pos in
    let in_board (p, s) = Board.piece_at s board = Some p in
    let open Piece in
    let open Square in
    let cr = pos.castling_rights in
    let white_can_castle = cr.white_short || cr.white_long in
    let black_can_castle = cr.black_short || cr.black_long in
    let nb_wP, nb_bP, wNs, bNs, nb_wR, nb_bR =
      SquareMap.fold
        (fun s p (nb_wP, nb_bP, wNs, bNs, nb_wR, nb_bR) ->
          if p = wP then (nb_wP + 1, nb_bP, wNs, bNs, nb_wR, nb_bR)
          else if p = bP then (nb_wP, nb_bP + 1, wNs, bNs, nb_wR, nb_bR)
          else if p = wN then (nb_wP, nb_bP, s :: wNs, bNs, nb_wR, nb_bR)
          else if p = bN then (nb_wP, nb_bP, wNs, s :: bNs, nb_wR, nb_bR)
          else if p = wR then (nb_wP, nb_bP, wNs, bNs, nb_wR + 1, nb_bR)
          else if p = bR then (nb_wP, nb_bP, wNs, bNs, nb_wR, nb_bR + 1)
          else (nb_wP, nb_bP, wNs, bNs, nb_wR, nb_bR))
        board (0, 0, [], [], 0, 0)
    in
    if
      nb_wP = 8 && nb_bP = 8 && List.length wNs = 2 && List.length bNs = 2
      (* && nb_wR = 2 && nb_bR = 2 *)
      (* && List.for_all in_board [ (wB, c1); (wK, e1) ] *)
      (* && List.for_all in_board [ (bB, c8); (bK, e8) ] *)
    then
      if
        (* Under these conditions we can determine whether the position
           is a vampire with simple static checks *)
        nb_wR = 2 && nb_bR = 2
        && in_board (wB, c1)
        && in_board (bB, c8)
        && List.for_all in_board
             [
               (wP, b2);
               (wP, c2);
               (wP, d2);
               (wP, e2);
               (wP, g2);
               (bP, b7);
               (bP, c7);
               (bP, d7);
               (bP, e7);
               (bP, g7);
             ]
        && List.exists in_board [ (wP, a2); (wP, a3) ]
        && List.exists in_board [ (wP, f2); (wP, f3) ]
        && List.exists in_board [ (wP, h2); (wP, h3) ]
        && List.exists in_board [ (bP, a7); (bP, a6) ]
        && List.exists in_board [ (bP, f7); (bP, f6) ]
        && List.exists in_board [ (bP, h7); (bP, h6) ]
        && (in_board (wP, f2) || white_can_castle)
        && (in_board (bP, f7) || black_can_castle)
        && (in_board (wB, f1) || cr.white_short)
        && (in_board (bB, f8) || cr.black_short)
      then
        (* We do not need parity checks, as the position is
           assumed to be legal *)
        (* let white_parity =
         *   1
         *   + List.length (List.filter Square.is_dark wNs)
         *   + (if in_board (wP, a3) then 1 else 0)
         *   + (if in_board (wP, f3) then 1 else 0)
         *   + (if in_board (wP, h3) then 1 else 0)
         *   + (if in_board (wR, a1) then 0 else 1)
         *   + if in_board (wR, h1) then 0 else 1
         * in
         * let black_parity =
         *   1
         *   + List.length (List.filter Square.is_dark bNs)
         *   + (if in_board (bP, a6) then 1 else 0)
         *   + (if in_board (bP, f6) then 1 else 0)
         *   + (if in_board (bP, h6) then 1 else 0)
         *   + (if in_board (bR, a8) then 0 else 1)
         *   + if in_board (bR, h8) then 0 else 1
         * in
         * let correct_turn =
         *   if white_parity mod 2 = black_parity mod 2 then
         *     Color.is_white pos.turn
         *   else Color.is_black pos.turn
         * in
         * Some correct_turn *)
        Some true
      else Some false
    else None

  (* Precondition: pos be legal *)
  let is_vampire pos =
    match quick_vampire_test pos with
    | Some b -> b
    | None -> not (is_legal @@ mirror_image pos)
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
        (* Format.printf "is_vampire? %s\n" fen; *)
        (* Format.print_flush (); *)
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
  let max_n = int_of_string Sys.argv.(1) in
  let open_goals = ref [] in
  let max_n_analyzed = ref 0 in
  for n = 0 to max_n do
    try
      let ic = open_in ("./vampires-depth-" ^ string_of_int n) in
      open_goals := [];
      max_n_analyzed := n;
      Format.printf "Fetching depth %d\n" n;
      Format.print_flush ();
      (try
         while true do
           let line = input_line ic in
           match String.split_on_char '|' line with
           | [ fen; label ] ->
               (* 1 is always the depth we give them when first found *)
               save fen (1, label = "vampire");
               if label = "vampire" then open_goals := fen :: !open_goals
           | _ -> assert false
         done
       with End_of_file -> ());
      close_in ic
    with _ ->
      Format.printf "Analyzing depth %d\n" n;
      Format.print_flush ();
      let oc = open_out ("./vampires-depth-" ^ string_of_int n) in
      (* let pos = Position.initial in       *)
      let pos =
        Position.of_fen
          "r1b1k2r/ppppp1pp/6n1/7p/8/8/PPPPPPPP/R1B1KBn1 w Qkq - ? 1"
      in
      if !open_goals = [] then find_vampires oc (n + 1) pos
      else
        List.iter
          (fun fen ->
            find_vampires oc (n - !max_n_analyzed + 1) (Position.of_fen fen))
          !open_goals;

      close_out oc
  done
