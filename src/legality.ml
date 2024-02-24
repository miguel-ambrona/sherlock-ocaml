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
       - captures : a map from squares to pairs of integers; [a2 -> (3, 5)]
                    means that the piece that started the game on a2 performed
                    at least 3 captures and at most 5 (bounds are inclusive).
                    The keys of this map can only be the squares in the
                    1st, 2nd, 7th and 8th ranks.
       - mobility : map from pieces to mobility graphs (graphs where nodes are
                    squares and arrows indicate the possible moves the piece
                    of interest may have performed during the game).
       - missing  : map from colors to the set of candidate missing pieces of
                    that color, represented by the square where they started.
       - tombs    : map from colors to list of squares where the player of that
                    color has for sure captured enemy pieces. Tombs are stored
                    as a list (instead of a set) to allow for duplicates.
       - illegal  : flag indicating whether the position has been proven illegal
                    ("false" does not necessarily mean the position is legal).
  *)
  type t = {
    pos : Position.t;
    static : SquareSet.t;
    origins : SquareSet.t SquareMap.t;
    destinies : SquareSet.t SquareMap.t;
    captures : (int * int) SquareMap.t;
    mobility : Mobility.G.t PieceMap.t;
    missing : uncertain_square_set ColorMap.t;
    tombs : Square.t list ColorMap.t;
    illegal : string option;
  }

  let init pos =
    let all_squares = SquareSet.of_list Board.squares in
    {
      pos;
      static = SquareSet.empty;
      origins = SquareMap.empty;
      destinies =
        List.fold_left
          (fun acc r ->
            List.fold_left
              (fun acc s -> SquareMap.add s all_squares acc)
              acc
              (Board.Rank.relative r White |> Square.rank_squares))
          SquareMap.empty [ 1; 2; 7; 8 ];
      captures =
        List.fold_left
          (fun acc r ->
            List.fold_left
              (fun acc s -> SquareMap.add s (0, 15) acc)
              acc
              (Board.Rank.relative r White |> Square.rank_squares))
          SquareMap.empty [ 1; 2; 7; 8 ];
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
      tombs =
        Color.[ (White, []); (Black, []) ] |> List.to_seq |> ColorMap.of_seq;
      illegal = None;
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
    && SquareMap.equal ( = ) s1.captures s2.captures
    && ColorMap.equal equal_uncertain_square_set s1.missing s2.missing
    && Option.equal String.equal s1.illegal s2.illegal
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

  (* [path_from_origin ~state origin target] provides a path from [origin] to
     [target] minimizing the number of capture in case the moving piece is a
     pawn. If the path is impossible, this function returns None.

     The moving piece is the piece that starts the game in [origin].
     If this piece is a pawn and [resulting_pt = Some pt] with [pt <> Pawn],
     the pawn must promote to the specified piece type before getting to
     [target], if possible.
     [edge_filter] is a function from edge to bool that indicates whether the
     edge should be considered in the analysis (true) or not (false).

     When a path is found, this function returns the number of pawn captures in
     the path as a second argument. *)
  let path_from_origin ~(resulting_pt : Piece.piece_type option)
      ?(edge_filter = Fun.const true) ~(state : State.t) origin target =
    let path = Mobility.path ~edge_filter in
    let p = Position.piece_at_exn origin Position.initial in
    let p_graph = PieceMap.find p state.mobility in
    let c = Piece.color p in
    match Piece.piece_type p with
    | Pawn -> (
        let eight_rank = Square.rank_squares @@ Board.Rank.relative 8 c in
        let select_shortest ~init options =
          List.fold_left
            (fun acc (p, d) ->
              match acc with
              | Some (_, d') when d' <= d -> acc
              | _ -> Some (p, d))
            init options
        in
        match resulting_pt with
        | Some Pawn -> path p_graph origin target
        | Some pt ->
            let t_graph = PieceMap.find (Piece.make c pt) state.mobility in
            List.filter_map
              (fun prom_sq ->
                let until_prom = path p_graph origin prom_sq in
                let after_prom = path t_graph prom_sq target in
                match (until_prom, after_prom) with
                | Some (p1, d1), Some (p2, d2) -> Some (p1 @ p2, d1 + d2)
                | _ -> None)
              eight_rank
            |> select_shortest ~init:None
        | None ->
            (* Given all the freedom with promotion types, we assume that once
               on the eight_rank the piece may go anywhere *)
            List.filter_map
              (fun prom_sq -> path p_graph origin prom_sq)
              eight_rank
            |> select_shortest ~init:(path p_graph origin target))
    | pt ->
        if resulting_pt <> None && resulting_pt <> Some pt then None
        else path p_graph origin target

  (* Returns a set of squares where a capture must have taken place for the
     piece that started the game in [origin] to reach square [target].

     If this piece is a pawn and [resulting_pt = Some pt] with [pt <> Pawn],
     the pawn must promote to the specified piece type before getting to
     [target], if possible. *)
  let capturing_squares_in_path ~(resulting_pt : Piece.piece_type option)
      ~(state : State.t) origin target =
    match path_from_origin ~resulting_pt ~state origin target with
    | None -> SquareSet.empty
    | Some (_, 0) -> SquareSet.empty
    | Some (path, _) ->
        let _, bound = SquareMap.find origin state.captures in
        List.filter_map
          (fun (_, w, t) ->
            if w <> 1 then None
            else
              let edge_filter (_, w', t') = not (w' = 1 && t' = t) in
              match
                path_from_origin ~edge_filter ~resulting_pt ~state origin target
              with
              | None -> Some t
              | Some (_, d) when d > bound -> Some t
              | _ -> None)
          path
        |> SquareSet.of_list

  (* If the set cardinal matches the total number of possible elements,
     we have found them all. *)
  let update_uncertain_square_set (usset : State.uncertain_square_set) =
    let all = SquareSet.union usset.definite usset.candidates in
    let found_all = usset.cardinal = SquareSet.cardinal all in
    let definite = if found_all then all else usset.definite in
    let candidates = SquareSet.diff usset.candidates definite in
    { usset with definite; candidates }

  (* Computes a lower bound on the number of captures to reach a pawn
     structure encoded as a list of 8 integers, one for each file, indicating
     the number of pawns in that file. *)
  let rec min_nb_captures_for_pawn_structure pawns_per_file =
    let labeled_files = List.mapi (fun i n -> (i, n)) pawns_per_file in
    let empty_files = List.filter (fun (_, n) -> n = 0) labeled_files in
    let busy_files = List.filter (fun (_, n) -> n > 1) labeled_files in
    match busy_files with
    | [] -> 0
    | (i, _) :: _ ->
        let free_on_the_left = List.filter (fun (j, _) -> j < i) empty_files in
        let free_on_the_right = List.filter (fun (j, _) -> j > i) empty_files in
        let min_nb_captures_if_left =
          match List.rev free_on_the_left with
          | [] -> 16
          | (j, _) :: _ ->
              let array = Array.of_list pawns_per_file in
              array.(j) <- 1;
              array.(i) <- array.(i) - 1;
              i - j + min_nb_captures_for_pawn_structure (Array.to_list array)
        in
        let min_nb_captures_if_right =
          match free_on_the_right with
          | [] -> 16
          | (j, _) :: _ ->
              let array = Array.of_list pawns_per_file in
              array.(j) <- 1;
              array.(i) <- array.(i) - 1;
              j - i + min_nb_captures_for_pawn_structure (Array.to_list array)
        in
        min min_nb_captures_if_left min_nb_captures_if_right

  let string_of_square_set set =
    SquareSet.elements set |> List.map Square.to_string |> String.concat ", "
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
        if
          Board.piece_at s Board.initial = Some p
          && List.for_all (is_static ~state) (Helpers.predecessors p s)
        then { state with static = SquareSet.add s state.static }
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
    then { state with illegal = Some "material" }
    else state

  (* Computes a bound on the minimum number of captures needed to reach
     the current pawn structure in the position, it updates the [captures]
     field accordingly. *)
  let pawn_structure_rule state =
    let pawns_per_file c =
      let array = [| 0; 0; 0; 0; 0; 0; 0; 0 |] in
      List.iter
        (fun (p, s) ->
          if Piece.piece_type p = Piece.pawn then
            array.(Square.file s) <- array.(Square.file s) + 1)
        (Position.color_pieces c state.pos);
      Array.to_list array
    in
    let min_nb_officer_captures_by c =
      SquareMap.fold
        (fun s (lower, _upper) cnt ->
          if Square.in_relative_rank 8 c s then cnt + lower else cnt)
        state.captures 0
    in
    let white_pawns = pawns_per_file Color.White in
    let black_pawns = pawns_per_file Color.Black in
    let min_nb_pawn_captures_by_white =
      Helpers.min_nb_captures_for_pawn_structure white_pawns
    in
    let min_nb_pawn_captures_by_black =
      Helpers.min_nb_captures_for_pawn_structure black_pawns
    in
    let min_captured_by_white =
      min_nb_pawn_captures_by_white + min_nb_officer_captures_by Color.White
    in
    let min_captured_by_black =
      min_nb_pawn_captures_by_black + min_nb_officer_captures_by Color.Black
    in
    let nb_white_missing = 16 - List.length (Position.white_pieces state.pos) in
    let nb_black_missing = 16 - List.length (Position.black_pieces state.pos) in
    if
      min_captured_by_white > nb_black_missing
      || min_captured_by_black > nb_white_missing
    then { state with illegal = Some "pawn structure" }
    else
      let captures =
        SquareMap.mapi
          (fun s (lower, upper) ->
            if Square.in_relative_rank 1 Color.White s then
              ( lower,
                min upper (nb_black_missing - min_nb_pawn_captures_by_white) )
            else if Square.in_relative_rank 1 Color.Black s then
              ( lower,
                min upper (nb_white_missing - min_nb_pawn_captures_by_black) )
            else (lower, upper))
          state.captures
      in
      { state with captures }

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
      let is_illegal =
        ColorMap.exists
          (fun _ usset -> not SquareSet.(is_empty (inter usset.definite to_rm)))
          state.missing
        || SquareMap.exists (fun _ os -> SquareSet.is_empty os) state.origins
      in
      let illegal =
        match state.illegal with
        | None when is_illegal -> Some "refine origins rule"
        | _ -> state.illegal
      in
      { state with origins; missing; illegal }
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
        | -1 -> { state with illegal = Some "refine origins rule" }
        | 0 -> remove_origins ids set state
        | _ -> state)
      state groups

  (* If the piece currently on square s has only one candidate origin o,
     then we can claim that the destiny of o is s.
     If s is the origin square of a missing piece, its candidate destinies are,
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
    let destinies =
      let is_reachable o t =
        let _, bound = SquareMap.find o state.captures in
        match Helpers.path_from_origin ~resulting_pt:None ~state o t with
        | None -> false
        | Some (_, d) -> d <= bound
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
      let knights =
        SquareMap.filter (fun _ set -> SquareSet.subset bi_or_gi set) origins
      in
      let knight_locs = SquareMap.bindings knights |> List.map fst in
      if knight_locs = [] then origins
      else
        (* First, make sure that all knights are connected, without having to
           retract pawns *)
        let connected =
          let first_loc = List.hd knight_locs in
          let squares_to_avoid =
            List.filter
              (fun (p, _) -> Piece.piece_type p = Piece.pawn)
              (Position.pieces state.pos)
            |> List.map snd |> SquareSet.of_list
          in
          let edge_filter (s, _, t) =
            not
              (SquareSet.mem s squares_to_avoid
              && SquareSet.mem t squares_to_avoid)
          in
          let knight_graph =
            PieceMap.find
              (Position.piece_at_exn first_loc state.pos)
              state.mobility
          in
          let connected s t =
            Option.is_some @@ Mobility.path ~edge_filter knight_graph s t
          in
          List.fold_left
            (fun (acc, s1) s2 -> (acc && connected s1 s2, s2))
            (true, first_loc) (List.tl knight_locs)
          |> fst
        in
        (* If they are not connected, but there are 2 candidate origins in all
           knights, we can also assume wlog their location *)
        if
          (not connected)
          && SquareMap.exists (fun _ set -> SquareSet.cardinal set <> 2) knights
        then origins
        else
          let knight_origins =
            SquareMap.fold (fun _ -> SquareSet.union) knights SquareSet.empty
          in
          match
            SquareSet.cardinal knight_origins - SquareMap.cardinal knights
          with
          | 0 ->
              origins
              |> SquareMap.add (List.nth knight_locs 0) (SquareSet.singleton bi)
              |> SquareMap.add (List.nth knight_locs 1) (SquareSet.singleton gi)
          | 1 ->
              origins
              |> SquareMap.add (List.nth knight_locs 0) (SquareSet.singleton bi)
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
    let mobility =
      PieceMap.mapi
        (fun p g ->
          let king_square = if Piece.is_white p then Square.e8 else Square.e1 in
          if not (SquareSet.mem king_square state.static) then g
          else
            let remove_edges g s =
              if Piece.piece_type p = King then
                Mobility.filter_edges (fun o t -> o <> s && t <> s) g
              else Mobility.filter_edges (fun o _ -> o <> s) g
            in
            Helpers.predecessors ~captures_only:true p king_square
            |> List.fold_left remove_edges g)
        state.mobility
    in
    { state with mobility }

  (* If the king is static we can empty its mobility graph. *)
  let refine_static_king_rule state =
    let mobility =
      PieceMap.mapi
        (fun p g ->
          let king_square = if Piece.is_white p then Square.e1 else Square.e8 in
          if
            (not (SquareSet.mem king_square state.static))
            || Piece.piece_type p <> King
          then g
          else Mobility.G.empty)
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
          let p = Position.piece_at_exn s state.pos in
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
    let origins =
      SquareMap.mapi
        (fun s ->
          let pt = Piece.piece_type (Position.piece_at_exn s state.pos) in
          SquareSet.filter (fun o ->
              let _, bound = SquareMap.find o state.captures in
              match
                Helpers.path_from_origin ~resulting_pt:(Some pt) ~state o s
              with
              | None -> false
              | Some (_, d) -> d <= bound))
        state.origins
    in
    { state with origins }

  (* We can fill the captures field for every piece based on the minimum
     distance from its candidate origins, since distance measures number
     of captures necessary to navigate over the mobility graph. *)
  let captures_rule state =
    let captures =
      SquareMap.fold
        (fun o destinies captures ->
          let lower =
            SquareSet.elements destinies
            |> List.map (fun t ->
                   let resulting_pt =
                     match SquareMap.find_opt t state.origins with
                     | Some set when SquareSet.cardinal set = 1 ->
                         if SquareSet.mem o set then
                           let p = Position.piece_at_exn t state.pos in
                           Some (Piece.piece_type p)
                         else None
                     | _ -> None
                   in
                   match Helpers.path_from_origin ~resulting_pt ~state o t with
                   | None -> Int.max_int
                   | Some (_, d) -> d)
            |> List.fold_left min Int.max_int
          in
          if lower < Int.max_int then
            let lower_o, upper_o = SquareMap.find o captures in
            SquareMap.add o (max lower_o lower, upper_o) captures
          else captures)
        state.destinies state.captures
    in
    let nb_white = List.length (Position.white_pieces state.pos) in
    let nb_black = List.length (Position.black_pieces state.pos) in
    (* white_cnt (resp. black_cnt) is a lower bound on the total number
       of captures performed by the white (resp. black) pieces *)
    let white_cnt, black_cnt =
      SquareMap.fold
        (fun o (lower, _) (white_cnt, black_cnt) ->
          if Piece.is_white @@ Board.(piece_at o initial |> Option.get) then
            (white_cnt + lower, black_cnt)
          else (white_cnt, black_cnt + lower))
        captures (0, 0)
    in
    let captures =
      SquareMap.mapi
        (fun o (lower, upper) ->
          let new_upper =
            if Piece.is_white @@ Board.(piece_at o initial |> Option.get) then
              16 - nb_black - white_cnt + lower
            else 16 - nb_white - black_cnt + lower
          in
          (lower, min upper new_upper))
        captures
    in
    { state with captures }

  (* The position is illegal if there are more captures required than
     pieces off the board. *)
  let too_many_captures_rule state =
    let nb_white = List.length (Position.white_pieces state.pos) in
    let nb_black = List.length (Position.black_pieces state.pos) in
    let lower_bound_captures_by_c c =
      let rank1 = Board.Rank.relative 1 c |> Square.rank_squares in
      let rank2 = Board.Rank.relative 2 c |> Square.rank_squares in
      List.fold_left
        (fun n s -> n + fst (SquareMap.find s state.captures))
        0 (rank1 @ rank2)
    in
    if
      lower_bound_captures_by_c Black > 16 - nb_white
      || lower_bound_captures_by_c White > 16 - nb_black
      || SquareMap.exists (fun _ (_, upper) -> upper < 0) state.captures
    then { state with illegal = Some "too many captures needed" }
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

  let tombs_rule state =
    let empty_tombs =
      Color.[ (White, []); (Black, []) ] |> List.to_seq |> ColorMap.of_seq
    in
    (* Tombs due to exsiting pieces *)
    let tombs =
      SquareMap.fold
        (fun s origins tombs ->
          let p = Position.piece_at_exn s state.pos in
          let pt = Piece.piece_type p in
          let c = Piece.color p in
          let p_tombs =
            SquareSet.fold
              (fun o ->
                SquareSet.inter
                  (Helpers.capturing_squares_in_path ~resulting_pt:(Some pt)
                     ~state o s))
              origins
              (SquareSet.of_list Board.squares)
            |> SquareSet.elements
          in
          ColorMap.add c (p_tombs @ ColorMap.find c tombs) tombs)
        state.origins empty_tombs
    in
    (* Tombs due to potentially missing pieces *)
    let tombs =
      let definitely_missing =
        SquareSet.union (ColorMap.find Color.White state.missing).definite
          (ColorMap.find Color.Black state.missing).definite
      in
      SquareMap.fold
        (fun o destinies tombs ->
          let c =
            if
              Square.in_relative_rank 1 Color.White o
              || Square.in_relative_rank 2 Color.White o
            then Color.White
            else Color.Black
          in
          if not (SquareSet.mem o definitely_missing) then tombs
          else
            let p_tombs =
              SquareSet.fold
                (fun t ->
                  SquareSet.inter
                    (Helpers.capturing_squares_in_path ~resulting_pt:None ~state
                       o t))
                destinies
                (SquareSet.of_list Board.squares)
              |> SquareSet.elements
            in
            ColorMap.add c (p_tombs @ ColorMap.find c tombs) tombs)
        state.destinies tombs
    in
    { state with tombs }

  (* It should be possible to assign to every tomb a missing piece that can
     actually reach that tomb. If it is not possible, the position is illegal.
     This way we can also deduce the destiny of some pieces. *)
  let visiting_tombs_rules state =
    let refine_destinies c state =
      let tombs = ColorMap.find (Color.negate c) state.tombs in
      let definitely_missing = (ColorMap.find c state.missing).definite in
      let candidate_missing = (ColorMap.find c state.missing).candidates in
      let missing_pieces =
        SquareSet.union definitely_missing candidate_missing
      in
      let assignments =
        List.map
          (fun tomb ->
            SquareMap.filter
              (fun o o_destinies ->
                (Square.in_relative_rank 1 c o || Square.in_relative_rank 2 c o)
                && SquareSet.mem tomb o_destinies
                && SquareSet.mem o missing_pieces)
              state.destinies
            |> SquareMap.bindings |> List.map fst |> SquareSet.of_list)
          tombs
      in
      List.fold_left
        (fun state (id_tombs, tomb_missing) ->
          let id_tombs = SquareSet.of_list id_tombs in
          match
            Int.compare
              (SquareSet.cardinal tomb_missing)
              (SquareSet.cardinal id_tombs)
          with
          | -1 ->
              let reason =
                Format.sprintf
                  "not enough %s pieces could have possibly reached the set of \
                   squares {%s} for being captured"
                  (if Color.is_white c then "white" else "black")
                  (Helpers.string_of_square_set id_tombs)
              in
              { state with illegal = Some reason }
          | 0 ->
              {
                state with
                destinies =
                  SquareMap.mapi
                    (fun o ts ->
                      if SquareSet.mem o tomb_missing then
                        SquareSet.inter id_tombs ts
                      else ts)
                    state.destinies;
              }
          | _ -> state)
        state
        (Helpers.k_groups @@ List.combine tombs assignments)
    in
    state |> refine_destinies Color.White |> refine_destinies Color.Black

  (* If the parity of the number of moves made by each side can be determined,
     the side to move must be consistent with such parity, if it is not, the
     position is illegal. *)
  let parity_rule state =
    (* Returns (Some b) if it can be determined that piece p, which started
       the game on square s has performed a number of moves whose parity
       coincides with b : int. It returns None otherwise. *)
    let parity_of p s =
      if SquareSet.mem s state.static then Some 0
      else
        match SquareMap.find_opt s state.destinies with
        | Some ts -> (
            (* Check that the parity is the same to all the candidate targets *)
            let parities =
              List.map
                (fun t -> Mobility.parity (PieceMap.find p state.mobility) s t)
                (SquareSet.to_seq ts |> List.of_seq)
            in
            match parities with
            | hd :: tl when List.for_all (fun parity -> parity = hd) tl -> hd
            | _ -> None)
        | None -> None
    in
    let parity_of_halfmoves =
      SquareMap.fold
        (fun s p acc ->
          match parity_of p s with
          | Some b -> Option.bind acc (fun n -> Some (n + b))
          | None -> None)
        Board.initial (Some 0)
    in
    match parity_of_halfmoves with
    | None -> state
    | Some n ->
        (* The parity of halfmoves is even iff it is White to move *)
        if (n + if Position.is_white_to_move state.pos then 1 else 0) mod 2 = 0
        then { state with illegal = Some "parity" }
        else state

  let all_rules =
    [
      static_rule;
      material_rule;
      pawn_structure_rule;
      origins_rule;
      refine_origins_rule;
      destinies_rule;
      knight_origins_rule;
      static_mobility_rule;
      static_king_rule;
      refine_static_king_rule;
      pawn_on_3rd_rank_rule;
      route_from_origin_rule;
      captures_rule;
      too_many_captures_rule;
      missing_rule;
      tombs_rule;
      visiting_tombs_rules;
      parity_rule;
    ]

  let rec apply state rules =
    (* if Option.is_some state.illegal then state *)
    (* else *)
    let new_state = List.fold_left (fun s r -> r s) state rules in
    if State.equal state new_state then state else apply new_state rules
end

let illegal_check pos = Position.(is_check (flip_turn pos))

let is_legal_aux pos =
  if illegal_check pos then false
  else
    let state = Rules.(apply (State.init pos) all_rules) in
    Option.is_none state.illegal

let has_limited_retractions pos =
  let rs = Retraction.pseudo_legal_retractions pos in
  not
  @@ List.exists
       (fun r ->
         Retraction.retracted_piece pos r <> Piece.pawn
         && List.exists (Fun.negate illegal_check) (Retraction.apply pos r))
       rs

module FenMap = Map.Make (String)

let table = ref FenMap.empty
let fetch fen = FenMap.find_opt fen !table
let save fen result = table := FenMap.add fen result !table

let dangerous_retractions pos =
  has_limited_retractions pos
  || has_limited_retractions (Position.flip_turn pos)

let rec is_legal pos =
  let fen = Position.to_fen { pos with fullmove_number = 0 } in
  match fetch fen with
  | Some res -> res
  | None ->
      (* Save false for now, we will rewrite this result *)
      save fen false;
      let res =
        if not @@ is_legal_aux pos then false
        else if dangerous_retractions pos then
          (* Format.printf "%s\n" @@ Position.to_fen pos; *)
          (* Format.print_flush (); *)
          List.exists is_legal (Retraction.retracted pos)
        else
          (* Format.printf "%s true!\n" @@ Position.to_fen pos; *)
          (* Format.print_flush (); *)
          true
      in
      save fen res;
      res
