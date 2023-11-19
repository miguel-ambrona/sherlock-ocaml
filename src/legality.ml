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
module ColorMap = Map.Make (Color)

module State = struct
  (* Type to reason about sets of squares that are not completely determined.
       - cardinal   : indicates the number of elements in the set.
       - definite   : is a set of squares that are definitely in the set.
       - candidates : is a set of squares that may be in the set.
     Squares that are not in definite nor in candidates are definitely not in
     the set. *)
  type uncertain_square_set = {
    cardinal : int;
    definite : SquareSet.t;
    candidates : SquareSet.t;
  }

  (* Proof state, containing all the information derived about the legality
     of the position of interest:
       - pos      : the position being analyzed.
       - static   : set of squares where there is a piece that has never moved.
       - origins  : potential candidate origins of pieces that are still on the
                    board. For example, [a4 -> {a2, b2}] means that the piece
                    currently on a4 started the game in either a2 or b2.
       - destinies: candidate location where pieces may have ended (either by
                    being captured or because they are currently there in pos).
                    The keys of this map can only be the squares in the
                    1st, 2nd, 7th and 8th ranks. For example, [a2 -> {b3, b6}]
                    means that the pawn that started on a2 ended in b3 or b6.
       - captures : lower bound on the number of captures performed by pieces
                    that are still on the board. For example, [a4 -> 2] means
                    that the piece currently on a4 has captured at least twice.
       - mobility : map from pieces to mobility graphs (graphs where nodes are
                    squares and arrows indicate the possible moves the piece
                    of interest may have performed during the game).
       - missing  : map from colors to the set of candidate missing pieces of
                    that color, represented by the square where they started.
       - illegal  : flag indicating whether the position has been proven illegal
                    ("false" does not necessarily mean the position is legal).
  *)
  type t = {
    pos : Position.t;
    static : SquareSet.t;
    origins : SquareSet.t SquareMap.t;
    destinies : SquareSet.t SquareMap.t;
    captures : int SquareMap.t;
    mobility : Mobility.G.t PieceMap.t;
    missing : uncertain_square_set ColorMap.t;
    illegal : bool;
  }

  let init pos =
    {
      pos;
      static = SquareSet.empty;
      origins = SquareMap.empty;
      destinies = SquareMap.empty;
      captures = SquareMap.empty;
      mobility =
        PieceMap.of_seq
          (List.to_seq
          @@ List.map (fun p -> (p, Mobility.piece_graph p)) Piece.all_pieces);
      missing =
        List.map
          (fun c ->
            let rank1 = Board.Rank.relative 1 c |> Square.rank_squares in
            let rank2 = Board.Rank.relative 2 c |> Square.rank_squares in
            let usset =
              {
                cardinal = 16 - List.length (Position.color_pieces c pos);
                definite = SquareSet.empty;
                candidates = rank1 @ rank2 |> SquareSet.of_list;
              }
            in
            (c, usset))
          Color.[ White; Black ]
        |> List.to_seq |> ColorMap.of_seq;
      illegal = false;
    }

  type perspective = Existing | Missing

  let equal_uncertain_square_set s1 s2 =
    s1.cardinal = s2.cardinal
    && SquareSet.equal s1.definite s2.definite
    && SquareSet.equal s1.candidates s2.candidates

  let equal s1 s2 =
    (* Our rules only remove edges, so diffs between graphs can be detected
       by comparing the number of edges. (There is no G.equal in ocamlgraph) *)
    let same_nb_edges g1 g2 = Mobility.G.(nb_edges g1 = nb_edges g2) in
    Position.equal s1.pos s2.pos
    && SquareSet.equal s1.static s2.static
    && SquareMap.equal SquareSet.equal s1.origins s2.origins
    && SquareMap.equal SquareSet.equal s1.destinies s2.destinies
    && PieceMap.equal same_nb_edges s1.mobility s2.mobility
    && SquareMap.equal Int.equal s1.captures s2.captures
    && ColorMap.equal equal_uncertain_square_set s1.missing s2.missing
    && Bool.equal s1.illegal s2.illegal
end

module Helpers = struct
  let predecessors ?(captures_only = false) piece s =
    let open Direction in
    match Piece.piece_type piece with
    | King | Queen -> diag_neighbors s @ straight_neighbors s
    | Rook -> straight_neighbors s
    | Bishop -> diag_neighbors s
    | Knight -> knight_neighbors s
    | Pawn ->
        pawn_backward_sources (Piece.color piece) s
        |> List.filter (fun t ->
               (not captures_only) || Square.file s <> Square.file t)

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

  (* [distance_from_origin ~infty ~state o t] is a lower bound on the
     number of captures needed for the piece standing on t to have come from the
     original square o.
     If such path is impossible, this function returns infty. *)
  let distance_from_origin ~infty ~(state : State.t) origin target =
    let distance = Mobility.distance ~infty in
    let p = Position.piece_at target state.pos |> Option.get in
    let p_graph = PieceMap.find p state.mobility in
    let c = Piece.color p in
    match Piece.piece_type p with
    | (Queen | Rook | Bishop | Knight) when Square.in_relative_rank 2 c origin
      ->
        (* the piece at target is promoted *)
        let pawn_graph = PieceMap.find (Piece.cP c) state.mobility in
        List.map
          (fun promotion ->
            distance pawn_graph origin promotion
            + distance p_graph promotion target)
          (Square.rank_squares (Board.Rank.relative 8 c))
        |> List.fold_left min infty
    | _ -> distance p_graph origin target

  (* [distance_to_target ~infty ~state o t] is a lower bound on the
     number of captures needed for the piece originally on o to reach square t.
     If such path is impossible, this function returns infty. *)
  let distance_to_target ~infty ~(state : State.t) origin target =
    let distance = Mobility.distance ~infty in
    let p = Board.piece_at origin Board.initial |> Option.get in
    let p_graph = PieceMap.find p state.mobility in
    let c = Piece.color p in
    match Piece.piece_type p with
    | Pawn ->
        (* We could reach target through a promotion. We assume that if the pawn
           can reach a promoting square, it can then go anywhere, as there is so
           much freedom with the promoted piece *)
        List.map
          (fun promotion -> distance p_graph origin promotion)
          (Square.rank_squares (Board.Rank.relative 8 c))
        |> List.fold_left min (distance p_graph origin target)
    | _ -> distance (PieceMap.find p state.mobility) origin target

  (* If the set cardinal matches the total number of possible elements,
     we have found them all. *)
  let update_uncertain_square_set (usset : State.uncertain_square_set) =
    let all = SquareSet.union usset.definite usset.candidates in
    let found_all = usset.cardinal = SquareSet.cardinal all in
    let definite = if found_all then all else usset.definite in
    let candidates = SquareSet.diff usset.candidates definite in
    { usset with definite; candidates }

  (* Returns a lower bound on the number of captures by White (respectively by
     Black) to reach the structure of the current position. Obviously, the
     actual number of captures is the number of missing pices, but here we
     attend to the captures that are necessary for piece mobility, e.g., a pawn
     which moved diagonally requires captures for doing so. *)
  let structural_necessary_captures (state : State.t) =
    SquareMap.fold
      (fun s n (acc_w, acc_b) ->
        if Position.white_piece_at s state.pos then (acc_w + n, acc_b)
        else (acc_w, acc_b + n))
      state.captures (0, 0)

  (* Create a map from squares to integers. Binding [s -> n] means that the
     piece associated to square s has performed at most n captures.
     This number is computed based on the number of missing pieces and the
     number of necessary captures for the mobility of some pawns.
     The piece associated to s is either the piece currently on s
     (if perspective = Existing) or the missing piece that started on s
     (if perspective = Missing).
  *)
  let build_nb_affordable_captures_map ~perspective (state : State.t) =
    let nb_white_missing = 16 - List.length (Position.white_pieces state.pos) in
    let nb_black_missing = 16 - List.length (Position.black_pieces state.pos) in
    let nb_necessary_captures_by_white, nb_necessary_captures_by_black =
      structural_necessary_captures state
    in
    match perspective with
    | State.Existing ->
        SquareMap.mapi
          (fun s n ->
            if Position.white_piece_at s state.pos then
              nb_black_missing - nb_necessary_captures_by_white + n
            else nb_white_missing - nb_necessary_captures_by_black + n)
          state.captures
    | State.Missing ->
        let white_bindings =
          (ColorMap.find Color.White state.missing).definite
          |> SquareSet.elements
          |> List.map (fun s ->
                 (s, nb_black_missing - nb_necessary_captures_by_white))
        in
        let black_bindings =
          (ColorMap.find Color.White state.missing).definite
          |> SquareSet.elements
          |> List.map (fun s ->
                 (s, nb_black_missing - nb_necessary_captures_by_white))
        in
        SquareMap.of_seq (List.to_seq @@ white_bindings @ black_bindings)
end

module Rules = struct
  open State

  (* Pawns on the 2nd rank are static, castling pieces are static if the
     corresponding castling rights are enabled and pieces whose movement is
     limited by static pieces are also static. *)
  let static_rule state =
    let open Square in
    (* Static pieces due to castling rights. *)
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
    (* Static marriage: king and queen are static if they are surrounded
       by static pieces, even without castling rights enabled. *)
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
    (* Static pieces due to restricted movements. *)
    List.fold_left
      (fun state (p, s) ->
        if List.for_all (is_static ~state) (Helpers.predecessors p s) then
          { state with static = SquareSet.add s state.static }
        else state)
      { state with static }
      (Position.pieces state.pos)

  (* This rule will be removed as it will be subsumed by others. *)
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

  (* The candidate origin squares of a piece are determined based on the
     initial position of chess. Queens, Rooks, Bishops or Knights may also
     come from the corresponding 2nd rank, as they may be promoted. *)
  let origins_rule state =
    let open Square in
    let piece_origins (p, s) =
      if SquareSet.mem s state.static then SquareSet.singleton s
      else
        let c = Piece.color p in
        let pick l1 l2 = if Color.is_white c then l1 else l2 in
        let rank2 = Square.rank_squares (Board.Rank.relative 2 c) in
        (match Piece.piece_type p with
        | King -> [ pick e1 e8 ]
        | Queen -> rank2 @ [ pick d1 d8 ]
        | Rook -> rank2 @ pick [ a1; h1 ] [ a8; h8 ]
        | Bishop when Square.is_light s -> rank2 @ [ pick f1 c8 ]
        | Bishop -> rank2 @ [ pick c1 f8 ]
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
    let remove_origins protected to_rm state =
      let origins =
        SquareMap.mapi
          (fun s ts ->
            if SquareSet.mem s protected then ts else SquareSet.diff ts to_rm)
          state.origins
      in
      let missing =
        ColorMap.map
          (fun usset ->
            { usset with candidates = SquareSet.diff usset.candidates to_rm })
          state.missing
      in
      let illegal =
        ColorMap.exists
          (fun _ usset -> not SquareSet.(is_empty (inter usset.definite to_rm)))
          state.missing
      in
      { state with origins; missing; illegal = state.illegal || illegal }
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
        | 0 -> remove_origins ids set state
        | _ -> state)
      state groups

  (* If the piece currently on square s has only one candidate origin o,
     then we can claim that the destiny of o is s.
     If s is the origin square of a missing piece, its candidate endings are,
     a priori, all the squares that this piece could have reached. *)
  let destinies_rule state =
    (* Destinies due to single candidate origin *)
    let destinies =
      SquareMap.fold
        (fun s s_origins destinies ->
          match SquareSet.to_seq s_origins |> List.of_seq with
          | [ o ] -> SquareMap.add o (SquareSet.singleton s) destinies
          | _ -> destinies)
        state.origins state.destinies
    in
    (* Missing pieces destinies *)
    let affordable_nb_captures_map =
      Helpers.build_nb_affordable_captures_map ~perspective:State.Missing state
    in
    let destinies =
      let is_reachable o t =
        let infty = 16 in
        let bound =
          match SquareMap.find_opt o affordable_nb_captures_map with
          | None -> infty - 1
          | Some b -> b
        in
        Helpers.distance_to_target ~infty ~state o t <= bound
      in
      let missing =
        ColorMap.fold
          (fun _ usset -> SquareSet.union usset.definite)
          state.missing SquareSet.empty
      in
      SquareSet.fold
        (fun o destinies ->
          let candidate_targets =
            let default = SquareSet.of_list Square.all_squares in
            Option.value ~default (SquareMap.find_opt o destinies)
            |> SquareSet.filter (fun t -> is_reachable o t)
          in
          SquareMap.add o candidate_targets destinies)
        missing destinies
    in
    { state with destinies }

  (* It is not possible to know with certainty whether a non-promoted white
     knight came from b1 or g1. (In this legality analysis we do not have the
     fullmove counter into account.) Consequently, we can assume without loss
     of generality that the knight came from either of the two squares. *)
  let knight_origins_rule state =
    let open Square in
    let assume_knight_origins_wlog (bi, gi) origins =
      let bi_or_gi = SquareSet.of_list [ bi; gi ] in
      let original_from_bi_or_gi =
        SquareMap.filter (fun _ set -> SquareSet.equal bi_or_gi set) origins
        |> SquareMap.bindings |> List.map fst
      in
      match original_from_bi_or_gi with
      | [ n1; n2 ] ->
          origins
          |> SquareMap.add n1 (SquareSet.singleton bi)
          |> SquareMap.add n2 (SquareSet.singleton gi)
      | _ -> origins
    in
    let origins =
      state.origins
      |> assume_knight_origins_wlog (b1, g1)
      |> assume_knight_origins_wlog (b8, g8)
    in
    { state with origins }

  (* If a piece is static, no piece has passed through its square. *)
  let static_mobility_rule state =
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

  (* Every piece that has checked a static king must have been captured, thus
     we can disable arrows that move from squares that check a static king. *)
  let static_king_rule state =
    let remove_edges_from g s = Mobility.filter_edges (fun o _ -> o <> s) g in
    let mobility =
      PieceMap.mapi
        (fun p g ->
          let king_square = if Piece.is_white p then Square.e8 else Square.e1 in
          if not (SquareSet.mem king_square state.static) then g
          else
            Helpers.predecessors ~captures_only:true p king_square
            |> List.fold_left remove_edges_from g)
        state.mobility
    in
    { state with mobility }

  (* If a pawn on the 3rd rank, say on square s, has a single candidate origin,
     say o, it is the only piece on the board that can possibly have moved
     across the squares s <-> o. For example, if a pawn on b3 comes from b2,
     no piece can possibly have moved from b1 to b5. *)
  let pawn_on_3rd_rank_rule state =
    let remove_arrows_passing_through (s1, s2) g =
      let f o t = not (Square.aligned o s1 t && Square.aligned o s2 t) in
      Mobility.filter_edges f g
    in
    let mobility =
      SquareMap.fold
        (fun s origins mobility ->
          let p = Position.piece_at s state.pos |> Option.get in
          if
            SquareSet.cardinal origins = 1
            && Piece.piece_type p = Pawn
            && Square.in_relative_rank 3 (Piece.color p) s
          then
            let o = SquareSet.choose origins in
            PieceMap.mapi
              (fun p' g ->
                if p' <> p then remove_arrows_passing_through (s, o) g else g)
              mobility
          else mobility)
        state.origins state.mobility
    in
    { state with mobility }

  (* There must exist a path to a piece from any of its candidate origins. *)
  let route_from_origin_rule state =
    let infty = 16 in
    let affordable_nb_captures_map =
      Helpers.build_nb_affordable_captures_map ~perspective:State.Existing state
    in
    let origins =
      SquareMap.mapi
        (fun s ->
          let bound =
            match SquareMap.find_opt s affordable_nb_captures_map with
            | None -> infty - 1
            | Some b -> b
          in
          SquareSet.filter (fun o ->
              Helpers.distance_from_origin ~infty ~state o s <= bound))
        state.origins
    in
    { state with origins }

  (* We can fill the captures field for every piece based on the minimum
     distance from its candidate origins, since distance measures number
     of captures necessary to navigate over the mobility graph. *)
  let captures_lower_bound_rule state =
    let infty = 16 in
    let captures =
      SquareMap.mapi
        (fun s origins ->
          SquareSet.elements origins
          |> List.map (fun o -> Helpers.distance_from_origin ~infty ~state o s)
          |> List.fold_left min infty)
        state.origins
    in
    { state with captures }

  (* The position is illegal if there are more captures required than
     pieces off the board. *)
  let too_many_captures_rule state =
    let nb_necessary_captures_by_white, nb_necessary_captures_by_black =
      Helpers.structural_necessary_captures state
    in
    let nb_white = List.length (Position.white_pieces state.pos) in
    let nb_black = List.length (Position.black_pieces state.pos) in
    if
      nb_necessary_captures_by_black > 16 - nb_white
      || nb_necessary_captures_by_white > 16 - nb_black
    then { state with illegal = true }
    else state

  (* The starting squares that do not appear in the origins of any piece on
     the board are definitely the starting position of missing pieces. *)
  let missing_rule state =
    let used_origins =
      SquareMap.fold (fun _ -> SquareSet.union) state.origins SquareSet.empty
    in
    let missing =
      ColorMap.map
        (fun usset ->
          let new_definite = SquareSet.diff usset.candidates used_origins in
          let definite = SquareSet.union usset.definite new_definite in
          { usset with definite } |> Helpers.update_uncertain_square_set)
        state.missing
    in
    { state with missing }

  let all_rules =
    [
      static_rule;
      material_rule;
      origins_rule;
      refine_origins_rule;
      destinies_rule;
      knight_origins_rule;
      static_mobility_rule;
      static_king_rule;
      pawn_on_3rd_rank_rule;
      route_from_origin_rule;
      captures_lower_bound_rule;
      too_many_captures_rule;
      missing_rule;
    ]

  let rec apply state rules =
    let new_state = List.fold_left (fun s r -> r s) state rules in
    if State.equal state new_state then state else apply new_state rules
end

let is_legal pos =
  let state = Rules.(apply (State.init pos) all_rules) in
  not state.illegal
