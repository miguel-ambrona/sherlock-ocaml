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

open Board

type t = {
  source : Square.t;
  target : Square.t;
  promoted : Piece.piece_type option;
}

let of_string str =
  let source = Square.of_string (String.sub str 0 2) in
  let target = Square.of_string (String.sub str 2 2) in
  let promoted =
    if String.length str < 5 then None
    else Some (Piece.piece_type_of_char str.[4])
  in
  { source; target; promoted }

let to_string m =
  let promotion_str =
    match m.promoted with
    | None -> ""
    | Some pt -> Piece.piece_type_to_char pt |> Char.escaped
  in
  Square.to_string m.source ^ Square.to_string m.target ^ promotion_str

let equal = ( = )
let source m = m.source
let target m = m.target
let promoted_piece m = m.promoted
let moved_piece pos m = Option.get (Position.piece_at m.source pos)

let is_en_passant pos m =
  Piece.piece_type (moved_piece pos m) = Piece.pawn
  && Square.file m.source <> Square.file m.target
  && (Option.is_none @@ Position.piece_at m.target pos)

let is_double_push pos m =
  Piece.piece_type (moved_piece pos m) = Piece.pawn
  && abs (Square.rank m.source - Square.rank m.target) = 2

let is_capture pos m =
  Option.is_some (Position.piece_at m.target pos) || is_en_passant pos m

let is_castling pos m =
  Piece.piece_type (moved_piece pos m) = Piece.king
  && Square.king_distance m.source m.target = 2

let apply pos m =
  let p = moved_piece pos m in
  let c = Piece.color p in
  let ep_square = Direction.relative_south c m.target in
  let remove_enemy_pawn_if_ep =
    if not @@ is_en_passant pos m then Fun.id
    else Board.remove_piece (Option.get ep_square)
  in
  let update_rook_if_castling board =
    let open Square in
    if not @@ is_castling pos m then board
    else
      let rook_source, rook_target =
        match c with
        | White when m.target = c1 -> (a1, d1)
        | White when m.target = g1 -> (h1, f1)
        | Black when m.target = c8 -> (a8, d8)
        | Black when m.target = g8 -> (h8, f8)
        | _ -> raise @@ Failure "apply: invalid castling move"
      in
      board
      |> Board.remove_piece rook_source
      |> Board.set_piece (Piece.cR c, rook_target)
  in
  let new_p = match m.promoted with None -> p | Some pt -> Piece.make c pt in
  let new_board =
    Position.board pos
    |> Board.remove_piece m.source
    |> Board.set_piece (new_p, m.target)
    |> remove_enemy_pawn_if_ep |> update_rook_if_castling
  in
  let castling_rights =
    let open Piece in
    let open Square in
    let cr = pos.castling_rights in
    let find p s = Board.piece_at s new_board = Some p in
    Position.
      {
        white_short = cr.white_short && find wK e1 && find wR h1;
        white_long = cr.white_long && find wK e1 && find wR a1;
        black_short = cr.black_short && find bK e8 && find bR h8;
        black_long = cr.black_long && find bK e8 && find bR a8;
      }
  in
  let increase_halfmove_clock n =
    if is_capture pos m || Piece.piece_type p = Piece.pawn then 0 else n + 1
  in
  Position.
    {
      board = new_board;
      turn = Color.negate pos.turn;
      en_passant = (if is_double_push pos m then ep_square else None);
      castling_rights;
      halfmove_clock = Option.map increase_halfmove_clock pos.halfmove_clock;
      fullmove_number = (pos.fullmove_number + if Color.is_black c then 1 else 0);
    }

let piece_moves pos (p, s) =
  let is_empty s = Option.is_none @@ Position.piece_at s pos in
  let contains_color_piece c s =
    match Position.piece_at s pos with
    | None -> false
    | Some p -> c = Piece.color p
  in
  let c = Piece.color p in
  let not_c = Color.negate c in
  let contains_ally = contains_color_piece c in
  let contains_enemy = contains_color_piece not_c in
  let rec follow_ray = function
    | [] -> []
    | s :: ray -> if is_empty s then s :: follow_ray ray else [ s ]
  in
  let targets =
    let open Direction in
    match Piece.piece_type p with
    | King -> king_neighbors s
    | Queen -> List.concat_map follow_ray (queen_rays s)
    | Rook -> List.concat_map follow_ray (rook_rays s)
    | Bishop -> List.concat_map follow_ray (bishop_rays s)
    | Knight -> knight_neighbors s
    | Pawn ->
        let not_blocked = is_empty (Option.get @@ relative_north c s) in
        let ok_push t = not_blocked && is_empty t in
        let ok_capt t = contains_enemy t || Position.en_passant pos = Some t in
        List.filter ok_push (pawn_push_targets c s)
        @ List.filter ok_capt (pawn_capture_targets c s)
  in
  let moves =
    List.filter (Fun.negate contains_ally) targets
    |> List.map (fun t -> { source = s; target = t; promoted = None })
  in
  (* Promotions *)
  let moves =
    if Piece.piece_type p = Piece.pawn && Square.in_relative_rank 7 c s then
      let pts = Piece.[ queen; rook; bishop; knight ] in
      let expand m = List.map (fun pt -> { m with promoted = Some pt }) pts in
      List.concat_map expand moves
    else moves
  in
  (* Castling *)
  let castling_move k1 k2 r1 must_be_empty cannot_be_attacked =
    if
      s = k1
      && Position.available_castling pos k2
      && Position.piece_at k1 pos = Some (Piece.cK c)
      && Position.piece_at r1 pos = Some (Piece.cR c)
      && List.for_all is_empty must_be_empty
      && not (List.exists (Position.is_attacked pos not_c) cannot_be_attacked)
    then Some { source = k1; target = k2; promoted = None }
    else None
  in
  let open Square in
  let king_castle =
    if Color.is_white c then castling_move e1 g1 h1 [ f1; g1 ] [ e1; f1; g1 ]
    else castling_move e8 g8 h8 [ f8; g8 ] [ e8; f8; g8 ]
  in
  let queen_castle =
    if Color.is_white c then
      castling_move e1 c1 a1 [ b1; c1; d1 ] [ e1; c1; d1 ]
    else castling_move e8 c8 a8 [ b8; c8; d8 ] [ e8; c8; d8 ]
  in
  Option.(to_list king_castle @ to_list queen_castle) @ moves

let pseudo_legal_moves pos =
  let pieces = Position.color_pieces (Position.turn pos) pos in
  List.concat_map (piece_moves pos) pieces

let legal_moves pos =
  List.filter (fun m ->
      let new_pos = apply pos m in
      not (Position.is_check { new_pos with turn = pos.turn }))
  @@ pseudo_legal_moves pos
