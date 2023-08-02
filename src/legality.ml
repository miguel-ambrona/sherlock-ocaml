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
module SquareSet = Set.Make (Square)
module SquareMap = Map.Make (Square)

module Helpers = struct
  let bishop_directions =
    Direction.[ north_east; north_west; south_east; south_west ]

  let rook_directions = Direction.[ north; south; east; west ]
  let queen_directions = bishop_directions @ bishop_directions
  let king_directions = queen_directions

  let piece_directions = function
    | Piece.King | Piece.Queen -> rook_directions @ bishop_directions
    | Piece.Rook -> rook_directions
    | Piece.Bishop -> bishop_directions
    | Piece.Knight | Piece.Pawn -> assert false

  let predecessors piece s =
    let open Direction in
    match Piece.piece_type piece with
    | King | Queen -> diag_neighbors s @ straight_neighbors s
    | Rook -> straight_neighbors s
    | Bishop -> diag_neighbors s
    | Knight -> knight_neighbors s
    | Pawn ->
        if Square.in_relative_rank 2 (Piece.color piece) s then []
        else
          let dirs =
            if Color.is_white (Piece.color piece) then
              [ south_west; south; south_east ]
            else [ north_west; north; north_east ]
          in
          List.filter_map (fun dir -> dir s) dirs
end

module State = struct
  type t = {
    pos : Position.t;
    static : SquareSet.t;
    origins : SquareSet.t SquareMap.t;
    illegal : bool;
  }

  let init pos =
    {
      pos;
      static = SquareSet.empty;
      origins = SquareMap.empty;
      illegal = false;
    }

  let equal s1 s2 =
    Position.equal s1.pos s2.pos
    && SquareSet.equal s1.static s2.static
    && SquareMap.equal SquareSet.equal s1.origins s2.origins
    && Bool.equal s1.illegal s2.illegal
end

module Rules = struct
  open State

  let static_rule state =
    let open Square in
    (* Static pieces due to castling rights *)
    let castling_static =
      let cr = state.pos.castling_rights in
      List.filter_map
        (fun (b, squares) -> if b then Some squares else None)
        [
          (cr.white_short, [ e1; h1 ]);
          (cr.black_short, [ e8; h8 ]);
          (cr.white_long, [ a1; e1 ]);
          (cr.black_long, [ a8; e8 ]);
        ]
      |> List.concat
    in
    let is_static ~state s = SquareSet.mem s state.static in
    (* Static marriage: king and queen are static if they are sourounded
       by static pieces, even without castling rights enabled *)
    let marriage_static =
      List.concat_map
        (fun (border, marriage) ->
          if List.for_all (is_static ~state) border then marriage else [])
        [
          ([ c1; c2; d2; e2; f2; f1 ], [ d1; e1 ]);
          ([ c8; c7; d7; e7; f7; f8 ], [ d8; e8 ]);
        ]
    in
    let static =
      castling_static @ marriage_static
      |> SquareSet.of_list
      |> SquareSet.union state.static
    in
    (* Static pieces due to restricted movements *)
    List.fold_left
      (fun state (p, s) ->
        if List.for_all (is_static ~state) (Helpers.predecessors p s) then
          { state with static = SquareSet.add s state.static }
        else state)
      { state with static }
      (Position.pieces state.pos)

  let material_rule state =
    let board = Position.board state.pos in
    let count ?square_color p = Board.count ?square_color p board in
    let in_board =
      List.for_all (fun (p_opt, s) -> Board.piece_at s board = p_opt)
    in
    let open Piece in
    let open Square in
    let nb_wPs = count wP in
    let nb_bPs = count bP in
    let wP = Some Piece.wP in
    let bP = Some Piece.bP in
    let wBc1 = if in_board [ (wP, b2); (wP, d2); (None, c1) ] then 0 else 1 in
    let wBf1 = if in_board [ (wP, e2); (wP, g2); (None, f1) ] then 0 else 1 in
    let bBc8 = if in_board [ (bP, b7); (bP, d7); (None, c8) ] then 0 else 1 in
    let bBf8 = if in_board [ (bP, e7); (bP, g7); (None, f8) ] then 0 else 1 in
    let lbound_promoted_white =
      max 0 (count wN - 2)
      + max 0 (count wB ~square_color:Color.white - wBf1)
      + max 0 (count wB ~square_color:Color.black - wBc1)
      + max 0 (count wR - 2)
      + max 0 (count wQ - 1)
    in
    let lbound_promoted_black =
      max 0 (count bN - 2)
      + max 0 (count bB ~square_color:Color.white - bBc8)
      + max 0 (count bB ~square_color:Color.black - bBf8)
      + max 0 (count bR - 2)
      + max 0 (count bQ - 1)
    in
    if
      count wK <> 1
      || count bK <> 1
      || nb_wPs > 8 || nb_bPs > 8
      || 8 - nb_wPs < lbound_promoted_white
      || 8 - nb_bPs < lbound_promoted_black
    then { state with illegal = true }
    else state

  let rec apply state rules =
    let rec aux state = function
      | [] -> state
      | rule :: rules -> aux (rule state) rules
    in
    let new_state = aux state rules in
    if State.equal state new_state then state else apply new_state rules
end

let is_legal pos =
  let state = Rules.(apply (State.init pos) [ static_rule; material_rule ]) in
  not state.illegal
