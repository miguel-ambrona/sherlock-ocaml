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
module PieceMap = Map.Make (Piece)

module Helpers = struct
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

  let pawn_candidate_origins color s =
    let snd_rank = Board.Rank.relative 2 color in
    let rank_distance_to_origin = abs (Square.rank s - snd_rank) in
    let candidate_origin_files =
      List.filter
        (fun file -> abs (file - Square.file s) <= rank_distance_to_origin)
        Board.files
    in
    List.map
      (fun file -> Square.of_file_and_rank file snd_rank)
      candidate_origin_files

  (* We say a set of at least k sets is a k-group iff their union results
     in at most k elements.
     Function k_groups takes a list of identifier-set pairs (id * set)
     and returns a list of k-groups for different values of k. Each k-group
     is represented by a list of (>= k) set identifiers paired with the
     corresponding set union of that group.
     For example,
       k_groups [('a', {1, 2, 3});
                 ('b', {2, 3, 4});
                 ('c', {1, 2, 3});
                 ('d', {1, 5});
                 ('e', {1, 3, 4})]
     returns
      [(['a'; 'b'; 'c'; 'e'], {1, 2, 3, 4})].

     It is guaranteed to return all k-groups of minimal cardinality in the
     following sense. If there exists a k-group containing a certain set S,
     such k-group will appear in the output unless another k'-group containing
     S, for k' < k, has already been considered. *)
  let k_groups sets =
    let open SquareSet in
    let n =
      List.filter (fun set -> cardinal set > 1) (List.map snd sets)
      |> List.fold_left union empty |> cardinal
    in
    let rec k_group k ids acc = function
      | [] -> if List.length ids >= k then [ (ids, acc) ] else []
      | (id, set) :: rest ->
          let acc' = union set acc in
          if equal acc acc' then k_group k (id :: ids) acc rest
          else if cardinal acc' > k then k_group k ids acc rest
          else k_group k (id :: ids) acc' rest @ k_group k ids acc rest
    in
    let rec find_minimal (id, set) sets k =
      match k_group k [ id ] set sets with
      | [] -> if k > n then [] else find_minimal (id, set) sets (k + 1)
      | l -> l
    in
    let rec aux groups = function
      | [] -> groups
      | (id, set) :: rest ->
          let gs = find_minimal (id, set) rest (SquareSet.cardinal set) in
          aux (gs @ groups) rest
    in
    let compare_cardinals s1 s2 = Int.compare (cardinal s1) (cardinal s2) in
    aux [] @@ List.sort (fun (_, s1) (_, s2) -> compare_cardinals s1 s2) sets
end

module State = struct
  type t = {
    pos : Position.t;
    static : SquareSet.t;
    origins : SquareSet.t SquareMap.t;
    captures : int SquareMap.t;
    mobility : Mobility.G.t PieceMap.t;
    illegal : bool;
  }

  let init pos =
    {
      pos;
      static = SquareSet.empty;
      origins = SquareMap.empty;
      captures = SquareMap.empty;
      mobility =
        PieceMap.of_seq
          (List.to_seq
          @@ List.map (fun p -> (p, Mobility.piece_graph p)) Piece.all_pieces);
      illegal = false;
    }

  let equal s1 s2 =
    (* Our rules only remove edges, so diffs between graphs can be detected
       by comparing the number of edges. (There is no G.equal in ocamlgraph) *)
    let same_nb_edges g1 g2 = Mobility.G.(nb_edges g1 = nb_edges g2) in
    Position.equal s1.pos s2.pos
    && SquareSet.equal s1.static s2.static
    && SquareMap.equal SquareSet.equal s1.origins s2.origins
    && PieceMap.equal same_nb_edges s1.mobility s2.mobility
    && SquareMap.equal Int.equal s1.captures s2.captures
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

  let origins_rule state =
    let open Square in
    let piece_origins (p, s) =
      if SquareSet.mem s state.static then SquareSet.singleton s
      else
        let c = Piece.color p in
        let pick l1 l2 = if Color.is_white c then l1 else l2 in
        let rank2 = Square.rank_squares (Board.Rank.relative 2 c) in
        (match Piece.piece_type p with
        | King -> pick [ e1 ] [ e8 ]
        | Queen -> rank2 @ pick [ d1 ] [ d8 ]
        | Rook -> rank2 @ pick [ a1; h1 ] [ a8; h8 ]
        | Bishop when Square.is_light s -> rank2 @ pick [ f1 ] [ c8 ]
        | Bishop -> rank2 @ pick [ c1 ] [ f8 ]
        | Knight -> rank2 @ pick [ b1; g1 ] [ b8; g8 ]
        | Pawn -> Helpers.pawn_candidate_origins c s)
        |> SquareSet.of_list
    in
    List.fold_left
      (fun state (p, s) ->
        let ts =
          match SquareMap.find_opt s state.origins with
          | None -> piece_origins (p, s)
          | Some ts -> SquareSet.inter ts @@ piece_origins (p, s)
        in
        { state with origins = SquareMap.add s ts state.origins })
      state
      (Position.pieces state.pos)

  (* The refine_origins rule is extremely powerful and it can subsume the
     current material_rule. It is based on the idea that if there is a
     group of k pieces whose united set of candidate origins, S, has
     cardinality k, then we can safely remove S from the candidate origins of
     any other piece. Furthermore, if |S| < k, the position is illegal. *)
  let refine_origins_rule state =
    let remove_origins protected to_rm =
      SquareMap.mapi (fun s ts ->
          if SquareSet.mem s protected then ts else SquareSet.diff ts to_rm)
    in
    let groups =
      let is_white (s, _) = Position.white_piece_at s state.pos in
      let ws, bs = List.partition is_white (SquareMap.bindings state.origins) in
      Helpers.k_groups ws @ Helpers.k_groups bs
    in
    List.fold_left
      (fun state (ids, set) ->
        let ids = SquareSet.of_list ids in
        match Int.compare (SquareSet.cardinal set) (SquareSet.cardinal ids) with
        | -1 -> { state with illegal = true }
        | 0 -> { state with origins = remove_origins ids set state.origins }
        | _ -> state)
      state groups

  let mobility_rule state =
    (* if a piece is static, no piece has moved from its square *)
    let remove_arrows_passing_through s g =
      let f o t = s <> o && s <> t && not (Square.aligned o s t) in
      Mobility.filter_edges f g
    in
    let mobility =
      SquareSet.fold
        (fun s -> PieceMap.map (remove_arrows_passing_through s))
        state.static state.mobility
    in
    { state with mobility }

  let paths_rule state =
    (* there must exist a path to a piece from any of its candidate origins *)
    let feasible_origin target origin =
      let p = Position.piece_at target state.pos |> Option.get in
      let p_graph = PieceMap.find p state.mobility in
      let c = Piece.color p in
      match Piece.piece_type p with
      | (Queen | Rook | Bishop | Knight) when Square.in_relative_rank 2 c origin
        ->
          (* the piece at target is promoted *)
          let pawn_graph = PieceMap.find (Piece.cP c) state.mobility in
          List.exists
            (fun promotion ->
              Mobility.connected pawn_graph origin promotion
              && Mobility.connected p_graph promotion target)
            (Square.rank_squares (Board.Rank.relative 8 c))
      | _ -> Mobility.connected p_graph origin target
    in
    let origins =
      SquareMap.mapi
        (fun s -> SquareSet.filter (feasible_origin s))
        state.origins
    in
    { state with origins }

  let all_rules =
    [
      static_rule;
      material_rule;
      origins_rule;
      refine_origins_rule;
      mobility_rule;
      paths_rule;
    ]

  let rec apply state rules =
    let rec aux state = function
      | [] -> state
      | rule :: rules -> aux (rule state) rules
    in
    let new_state = aux state rules in
    if State.equal state new_state then state else apply new_state rules
end

let is_legal pos =
  let state = Rules.(apply (State.init pos) all_rules) in
  not state.illegal
