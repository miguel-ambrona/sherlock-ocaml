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

type retraction = Normal | UnPromotion | UnEnPassant

type t = {
  source : Square.t;
  target : Square.t;
  retraction_type : retraction;
  (* possible uncaptured piece excluding en passant pawns *)
  uncaptured : Piece.piece_type option;
}

let of_string str =
  let source = Square.of_string (String.sub str 0 2) in
  let uncaptured, shift =
    if str.[2] <> 'x' then (None, 0)
    else (Some (Piece.piece_type_of_char @@ Char.lowercase_ascii str.[3]), 2)
  in
  let target = Square.of_string (String.sub str (shift + 2) 2) in
  let retraction_type =
    if String.length str < 5 + shift then Normal
    else if "ep" = String.sub str (shift + 4) 2 then UnEnPassant
    else if "prom" = String.sub str (shift + 4) 4 then UnPromotion
    else Normal
  in
  { source; target; retraction_type; uncaptured }

let to_string r =
  let capitalize c = Char.escaped c |> String.capitalize_ascii in
  let captured_str =
    match r.uncaptured with
    | None -> ""
    | Some pt -> "x" ^ capitalize @@ Piece.piece_type_to_char pt
  in
  let retraction_type_str =
    match r.retraction_type with
    | Normal -> ""
    | UnPromotion -> "prom"
    | UnEnPassant -> "ep"
  in
  Square.to_string r.source ^ captured_str ^ Square.to_string r.target
  ^ retraction_type_str

let equal = ( = )
let source r = r.source
let target r = r.target
let is_unpromotion r = r.retraction_type = UnPromotion
let is_unenpassant r = r.retraction_type = UnEnPassant
let is_uncapture r = Option.is_some r.uncaptured || is_unenpassant r
let uncaptured_piece r = r.uncaptured

let retracted_piece pos r =
  if r.retraction_type = UnPromotion then Piece.pawn
  else Piece.piece_type @@ Option.get (Position.piece_at r.target pos)

let is_double_unpush pos r =
  retracted_piece pos r = Piece.pawn
  && Square.king_distance r.source r.target = 2

let is_uncastling pos r =
  retracted_piece pos r = Piece.king
  && Square.king_distance r.source r.target = 2

(* This function assumes that the given retraction is valid.
   It is the responsibility of [piece_retractions] to generate
   valid retractions.
   Also, unlike Move.apply, this function returns a list of positions
   instead of just one. This is because we must consider all possible
   castling rights and en passant flags after the retraction if applicable. *)
let apply pos r =
  let not_c = Position.turn pos in
  let c = Color.negate not_c in
  let p = Piece.make c @@ retracted_piece pos r in
  let ep_square = Direction.relative_south c r.target in
  let set_unenpassant board =
    if r.retraction_type <> UnEnPassant then board
    else Board.set_piece (Piece.cP not_c, Option.get ep_square) board
  in
  let set_uncaptured board =
    match r.uncaptured with
    | None -> board
    | Some pt -> Board.set_piece (Piece.make not_c pt, r.target) board
  in
  let is_uncastling = is_uncastling pos r in
  let update_uncastling_rook board =
    let open Square in
    if not is_uncastling then board
    else
      let rook_source, rook_target =
        match c with
        | White when r.target = c1 -> (a1, d1)
        | White when r.target = g1 -> (h1, f1)
        | Black when r.target = c8 -> (a8, d8)
        | Black when r.target = g8 -> (h8, f8)
        | _ -> raise @@ Failure "apply: invalid castling retraction"
      in
      board
      |> Board.remove_piece rook_target
      |> Board.set_piece (Piece.cR c, rook_source)
  in
  let new_board =
    Position.board pos
    |> Board.remove_piece r.target
    |> Board.set_piece (p, r.source)
    |> set_unenpassant |> set_uncaptured |> update_uncastling_rook
  in
  let new_candidate_castling_rights =
    let open Piece in
    let open Square in
    let open Position in
    let is_uncastling_white_short = is_uncastling && r.target = g1 in
    let is_uncastling_white_long = is_uncastling && r.target = c1 in
    let is_uncastling_black_short = is_uncastling && r.target = g8 in
    let is_uncastling_black_long = is_uncastling && r.target = c8 in
    let cr = pos.castling_rights in
    let find board (p, s) = Board.piece_at s board = Some p in
    let there_was = List.for_all (find pos.board) in
    let there_is = List.for_all (find new_board) in
    let rec all_castling_rights variations = function
      | [] -> List.map List.rev variations
      | (false, true) :: rest ->
          all_castling_rights
            (List.map (fun vs -> false :: vs) variations
            @ List.map (fun vs -> true :: vs) variations)
            rest
      | (b, _) :: rest ->
          all_castling_rights (List.map (fun vs -> b :: vs) variations) rest
    in
    let ws_config = [ (wK, e1); (wR, h1) ] in
    let wl_config = [ (wK, e1); (wR, a1) ] in
    let bs_config = [ (bK, e8); (bR, h8) ] in
    let bl_config = [ (bK, e8); (bR, a8) ] in
    all_castling_rights [ [] ]
      [
        (cr.white_short, there_is ws_config && not (there_was ws_config));
        (cr.white_long, there_is wl_config && not (there_was wl_config));
        (cr.black_short, there_is bs_config && not (there_was bs_config));
        (cr.black_long, there_is bl_config && not (there_was bl_config));
      ]
    |> List.map (function
         | [ white_short; white_long; black_short; black_long ] ->
             { white_short; white_long; black_short; black_long }
         | _ -> assert false)
    |> List.filter (fun cr -> (not is_uncastling_white_short) || cr.white_short)
    |> List.filter (fun cr -> (not is_uncastling_white_long) || cr.white_long)
    |> List.filter (fun cr -> (not is_uncastling_black_short) || cr.black_short)
    |> List.filter (fun cr -> (not is_uncastling_black_long) || cr.black_long)
  in
  let candidate_ep_flags =
    match pos.halfmove_clock with
    | Some n when n >= 1 -> []
    | _ ->
        let pawn_at s c = Board.piece_at s new_board = Some (Piece.cP c) in
        let is_empty s = Option.is_none @@ Board.piece_at s new_board in
        let east s = Option.value ~default:(-1) (Direction.east s) in
        let west s = Option.value ~default:(-1) (Direction.west s) in
        let south s = Direction.relative_south_exn c s in
        let candidate_double_push s =
          pawn_at (south s) not_c
          && (pawn_at (east (south s)) c || pawn_at (west (south s)) c)
          && is_empty s
          && is_empty (Direction.relative_north_exn c s)
        in
        List.filter candidate_double_push
          (Square.rank_squares @@ Rank.relative 3 not_c)
  in
  let candidate_ep_flags =
    if is_unenpassant r then [ Some r.target ]
    else None :: List.map (fun s -> Some s) candidate_ep_flags
  in
  let decrease_halfmove_clock = function
    | Some n when n > 0 ->
        assert (not (is_uncapture r || Piece.piece_type p = Piece.pawn));
        Some (n - 1)
    | _ -> None
  in
  let new_pos =
    Position.
      {
        pos with
        board = new_board;
        turn = Color.negate pos.turn;
        en_passant = None;
        halfmove_clock = decrease_halfmove_clock pos.halfmove_clock;
        fullmove_number =
          (pos.fullmove_number - if Color.is_black c then 1 else 0);
      }
  in
  List.concat_map
    (fun en_passant ->
      let halfmove_clock =
        if en_passant = None then new_pos.halfmove_clock else Some 0
      in
      List.map
        (fun castling_rights ->
          { new_pos with en_passant; castling_rights; halfmove_clock })
        new_candidate_castling_rights)
    candidate_ep_flags

let piece_retractions pos (p, t) =
  let is_empty s = Option.is_none @@ Position.piece_at s pos in
  let c = Piece.color p in
  let not_c = Color.negate c in
  let rec follow_ray = function
    | [] -> []
    | s :: ray -> if is_empty s then s :: follow_ray ray else []
  in
  let pawn_single_unpush t =
    let s = Direction.relative_south_exn c t in
    if is_empty s && not (Square.in_relative_rank 1 c s) then [ s ] else []
  in
  let pawn_uncaptures t =
    List.filter is_empty (Direction.pawn_uncapture_sources c t)
  in
  let sources =
    let open Direction in
    match Piece.piece_type p with
    | King -> king_neighbors t
    | Queen -> List.concat_map follow_ray (queen_rays t)
    | Rook -> List.concat_map follow_ray (rook_rays t)
    | Bishop -> List.concat_map follow_ray (bishop_rays t)
    | Knight -> knight_neighbors t
    | Pawn -> []
  in
  let piece_normal_retractions =
    List.map
      (fun s ->
        { source = s; target = t; retraction_type = Normal; uncaptured = None })
      (List.filter is_empty sources)
  in
  let candidate_captured_types =
    (if Square.in_relative_rank 1 c t || Square.in_relative_rank 8 c t then []
     else [ Piece.pawn ])
    @ Piece.[ queen; rook; bishop; knight ]
  in
  (* Uncaptures *)
  let piece_uncaptures =
    List.concat_map
      (fun r ->
        List.map
          (fun pt -> { r with uncaptured = Some pt })
          candidate_captured_types)
      piece_normal_retractions
  in
  (* UnPromotions & pawn retractions *)
  let pawn_retractions retraction_type =
    let make source uncaptured =
      { source; target = t; retraction_type; uncaptured }
    in
    List.map (fun s -> make s None) (pawn_single_unpush t)
    @ List.concat_map
        (fun s ->
          List.map (fun pt -> make s (Some pt)) candidate_captured_types)
        (pawn_uncaptures t)
  in
  let unpromotions =
    if Square.in_relative_rank 8 c t && Piece.piece_type p <> Piece.king then
      pawn_retractions UnPromotion
    else []
  in
  let pawn_retractions =
    if Piece.piece_type p = Piece.pawn then pawn_retractions Normal else []
  in
  (* UnEnPassant *)
  let pawn_unenpassant =
    if
      Square.in_relative_rank 6 c t
      && Piece.piece_type p = Piece.pawn
      && is_empty (Direction.north_exn t)
      && is_empty (Direction.south_exn t)
    then
      List.map
        (fun source ->
          {
            source;
            target = t;
            retraction_type = UnEnPassant;
            uncaptured = None;
          })
        (pawn_uncaptures t)
    else []
  in
  (* Uncastling *)
  let castling_retraction k1 k2 r2 must_be_empty cannot_be_attacked =
    if
      t = k2
      && Position.piece_at k2 pos = Some (Piece.cK c)
      && Position.piece_at r2 pos = Some (Piece.cR c)
      && List.for_all is_empty must_be_empty
      && not (List.exists (Position.is_attacked pos not_c) cannot_be_attacked)
    then
      Some
        {
          source = k1;
          target = k2;
          retraction_type = Normal;
          uncaptured = None;
        }
    else None
  in
  let open Square in
  let king_uncastle =
    if Color.is_white c then
      castling_retraction e1 g1 f1 [ e1; h1 ] [ e1; f1; g1 ]
    else castling_retraction e8 g8 f8 [ e8; h8 ] [ e8; f8; g8 ]
  in
  let queen_uncastle =
    if Color.is_white c then
      castling_retraction e1 c1 d1 [ a1; b1; e1 ] [ e1; c1; d1 ]
    else castling_retraction e8 c8 d8 [ a8; b8; e8 ] [ e8; c8; d8 ]
  in
  let castling_retractions =
    Option.(to_list king_uncastle @ to_list queen_uncastle)
  in
  let cr = pos.castling_rights in
  match Position.en_passant pos with
  | Some ep_square ->
      let source = Direction.relative_south_exn c ep_square in
      assert (is_empty ep_square);
      assert (is_empty source);
      if
        Direction.relative_north_exn c ep_square = t
        && Piece.piece_type p = Piece.pawn
        && Option.value ~default:0 pos.halfmove_clock = 0
      then
        [ { source; target = t; retraction_type = Normal; uncaptured = None } ]
      else []
  | _ -> (
      if
        (t = a1 && cr.white_long)
        || (t = e1 && (cr.white_long || cr.white_short))
        || (t = h1 && cr.white_short)
        || (t = a8 && cr.black_long)
        || (t = e8 && (cr.black_long || cr.black_short))
        || (t = h8 && cr.black_short)
      then []
      else
        let resetting_retractions =
          piece_uncaptures @ unpromotions @ pawn_retractions @ pawn_unenpassant
        in
        let other_retractions =
          piece_normal_retractions @ castling_retractions
        in
        match pos.halfmove_clock with
        | Some n when n > 0 -> other_retractions
        | Some n when n = 0 -> resetting_retractions
        | _ -> other_retractions @ resetting_retractions)

(* All pseudo-legal retractions, i.e., the produced retractions are not
   guaranteed to lead to legal positions *)
let pseudo_legal_retractions pos =
  let pieces = Position.color_pieces (Color.negate @@ Position.turn pos) pos in
  List.concat_map (piece_retractions pos) pieces

(* Given a position, returns a list of retracted positions from it.
   We do not check for the legality of the resulting positions in terms of
   material or immaginary checks (or other more complex illegality patterns).
   However, this function avoids retracted positions where the player without
   the turn is in check. *)
let retracted pos =
  List.concat_map (fun r ->
      List.filter
        (fun new_pos ->
          not Position.(is_check { new_pos with turn = pos.turn }))
        (apply pos r))
  @@ pseudo_legal_retractions pos
