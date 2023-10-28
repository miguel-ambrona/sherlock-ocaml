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

module Piece = Board.Piece
module Square = Board.Square
module Direction = Board.Direction

type castling_rights = {
  white_short : bool;
  white_long : bool;
  black_short : bool;
  black_long : bool;
}

type t = {
  board : Board.t;
  turn : Color.t;
  castling_rights : castling_rights;
  en_passant : Square.t option;
  halfmove_clock : int option;
  fullmove_number : int;
}

let equal = ( = )

let castling_rights_of_string str =
  {
    white_short = String.contains str 'K';
    white_long = String.contains str 'Q';
    black_short = String.contains str 'k';
    black_long = String.contains str 'q';
  }

let castling_rights_to_string cr =
  let get b symbol = if b then symbol else "" in
  let out =
    String.concat ""
      [
        get cr.white_short "K";
        get cr.white_long "Q";
        get cr.black_short "k";
        get cr.black_long "q";
      ]
  in
  if String.length out = 0 then "-" else out

let en_passant_of_string = function
  | "-" -> None
  | s -> Some (Square.of_string s)

let en_passant_to_string = function None -> "-" | Some s -> Square.to_string s

let halfmove_clock_of_string = function
  | "?" -> None
  | s -> Some (int_of_string s)

let halfmove_clock_to_string = function
  | Some i when i >= 0 -> string_of_int i
  | _ -> "?"

let fullmove_number_of_string s = int_of_string s
let fullmove_number_to_string i = string_of_int i
let set_piece (p, s) pos = { pos with board = Board.set_piece (p, s) pos.board }
let remove_piece s pos = { pos with board = Board.remove_piece s pos.board }
let piece_at s pos = Board.piece_at s pos.board

let white_piece_at s pos =
  match piece_at s pos with None -> false | Some p -> Piece.is_white p

let sanitize_castling_rights board castling_rights =
  let open Square in
  {
    white_long =
      castling_rights.white_long
      && Board.piece_at e1 board = Some Piece.wK
      && Board.piece_at a1 board = Some Piece.wR;
    white_short =
      castling_rights.white_short
      && Board.piece_at e1 board = Some Piece.wK
      && Board.piece_at h1 board = Some Piece.wR;
    black_long =
      castling_rights.black_long
      && Board.piece_at e8 board = Some Piece.bK
      && Board.piece_at a8 board = Some Piece.bR;
    black_short =
      castling_rights.black_short
      && Board.piece_at e8 board = Some Piece.bK
      && Board.piece_at h8 board = Some Piece.bR;
  }

let sanitize_en_passant board turn = function
  | None -> None
  | Some s when not (Square.in_relative_rank 6 turn s) -> None
  | Some s ->
      let attacking_directions =
        if Color.is_white turn then Direction.[ south_east; south_west ]
        else Direction.[ north_east; north_west ]
      in
      let attacking_pawn = if Color.is_white turn then Piece.wP else Piece.bP in
      if
        List.exists
          (fun dir ->
            match dir s with
            | Some t -> Board.piece_at t board = Some attacking_pawn
            | None -> false)
          attacking_directions
      then Some s
      else None

let of_fen fen =
  let p1, p2, p3, p4, p5, p6 =
    match String.split_on_char ' ' fen with
    | [ p1; p2; p3; p4; p5; p6 ] -> (p1, p2, p3, p4, p5, p6)
    | _ ->
        raise @@ Invalid_argument "A valid FEN string must contain 6 components"
  in
  let board = Board.of_fen p1 in

  let turn = Color.of_char p2.[0] in
  {
    board;
    turn;
    (* We do not fail if enabled castling rights are not possible, we
       disable them instead. *)
    castling_rights =
      sanitize_castling_rights board @@ castling_rights_of_string p3;
    (* We remove the e.p. flag if there is no enemy pawn positioned to
       capture en passant. *)
    en_passant = sanitize_en_passant board turn @@ en_passant_of_string p4;
    halfmove_clock = halfmove_clock_of_string p5;
    fullmove_number = fullmove_number_of_string p6;
  }

let to_fen pos =
  String.concat " "
    [
      Board.to_fen pos.board;
      Char.escaped @@ Color.to_char pos.turn;
      castling_rights_to_string pos.castling_rights;
      en_passant_to_string pos.en_passant;
      halfmove_clock_to_string pos.halfmove_clock;
      fullmove_number_to_string pos.fullmove_number;
    ]

let board pos = pos.board
let turn pos = pos.turn
let en_passant pos = pos.en_passant
let pieces pos = Board.to_pieces pos.board
let white_pieces pos = List.filter (fun (p, _) -> Piece.is_white p) (pieces pos)
let black_pieces pos = List.filter (fun (p, _) -> Piece.is_black p) (pieces pos)

let color_pieces c pos =
  if Color.is_white c then white_pieces pos else black_pieces pos

let available_castling pos king_target =
  let cr = pos.castling_rights in
  if king_target = Square.c1 then cr.white_long
  else if king_target = Square.g1 then cr.white_short
  else if king_target = Square.c8 then cr.black_long
  else if king_target = Square.g8 then cr.black_short
  else raise @@ Invalid_argument "Invalid king target square"

let is_attacked pos c s =
  let open Direction in
  let rec ray_collisions = function
    | [] -> None
    | s :: ray ->
        if Option.is_some @@ piece_at s pos then Some s else ray_collisions ray
  in

  let straight_attackers = List.filter_map ray_collisions (rook_rays s) in
  let diagonal_attackers = List.filter_map ray_collisions (bishop_rays s) in
  let find p = List.exists (fun t -> piece_at t pos = Some p) in
  find (Piece.cK c) (king_neighbors s)
  || find (Piece.cQ c) (straight_attackers @ diagonal_attackers)
  || find (Piece.cR c) straight_attackers
  || find (Piece.cB c) diagonal_attackers
  || find (Piece.cN c) (knight_neighbors s)
  || find (Piece.cP c) (pawn_capture_targets (Color.negate c) s)

let is_check pos =
  match Board.find_pieces (Piece.cK pos.turn) pos.board with
  | [ king_sq ] -> is_attacked pos (Color.negate pos.turn) king_sq
  | _ ->
      raise @@ Invalid_argument "There must be exactly one king of each color"

let initial = of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
