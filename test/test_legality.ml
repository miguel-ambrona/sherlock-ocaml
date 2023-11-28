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

module Internal = struct
  open Legality
  open Board.Square

  type legality = Illegal | TBD

  let legality_assertion (state : State.t) = function
    | Illegal -> assert state.illegal
    | TBD -> assert (not state.illegal)

  let lacks_edge g (s, t) =
    try
      ignore @@ Mobility.G.find_edge g s t;
      false
    with Not_found -> true

  module TestHelpers = struct
    let test_pawn_candidate_origins () =
      List.iter
        (fun (c, s, expected_origins) ->
          assert (expected_origins = Helpers.pawn_candidate_origins c s))
        [
          (Color.white, e2, [ e2 ]);
          (Color.black, e2, [ a7; b7; c7; d7; e7; f7; g7; h7 ]);
          (Color.white, h6, [ d2; e2; f2; g2; h2 ]);
          (Color.black, h6, [ g7; h7 ]);
          (Color.white, b4, [ a2; b2; c2; d2 ]);
          (Color.black, b4, [ a7; b7; c7; d7; e7 ]);
        ]

    let test_k_groups () =
      let equivalent_ids ids1 ids2 =
        SquareSet.(equal (of_list ids1) (of_list ids2))
      in
      List.iter
        (fun (sets, expected) ->
          let groups =
            List.map (fun (id, l) -> (id, SquareSet.of_list l)) sets
            |> Helpers.k_groups |> List.map fst
          in
          assert (List.exists (equivalent_ids expected) groups))
        [
          ( [
              (1, [ a1; a2; a3 ]);
              (2, [ a2; a3; a4 ]);
              (3, [ a1; a2; a3 ]);
              (4, [ a1; a5 ]);
              (5, [ a1; a3; a4 ]);
            ],
            [ 1; 2; 3; 5 ] );
          ( [
              (1, [ b1; b2; b3; b4 ]);
              (2, [ b5; b6; b7; b8 ]);
              (3, [ b2; b4; b5; b1 ]);
              (4, [ b1; b2; b3; b4; b5 ]);
              (5, [ b2; b3; b8 ]);
              (6, [ b2; b3; b4; b5 ]);
              (7, [ b1; b3; b4; b5 ]);
            ],
            [ 1; 3; 4; 6; 7 ] );
        ]

    let test_path_from_origin () =
      List.iter
        (fun (fen, o, s, expected_distance) ->
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let bP_in_s s = Position.piece_at s pos = Some Piece.bP in
          let static = List.filter bP_in_s Board.squares in
          let state = State.init pos in
          let state = { state with static = SquareSet.of_list static } in
          let state = Rules.(apply state [ static_mobility_rule ]) in
          let resulting_pt =
            Position.piece_at s pos |> Option.map Piece.piece_type
          in
          let d =
            Option.map snd
            @@ Helpers.path_from_origin ~to_avoid:SquareSet.empty ~resulting_pt
                 ~state o s
          in
          assert (d = expected_distance))
        Square.
          [
            ("8/8/8/2B5/6p1/4pp2/8/8", f2, c5, Some 2);
            ("8/8/8/2B5/6p1/4pp2/8/8", h2, c5, Some 0);
            ("8/8/8/2B5/6p1/4pp2/8/8", c2, c5, Some 1);
            ("8/5p2/6pp/2B4p/5pp1/4pp2/8/8", f2, c5, Some 6);
            ("8/5p2/6pp/2Q4p/5pp1/4pp2/8/8", f2, c5, Some 5);
            ("5b2/8/8/8/8/8/8/8", f8, f8, Some 0);
            ("5b2/8/8/8/8/8/8/8", f7, f8, Some 1);
            ("5b2/8/8/8/8/8/8/8", g8, f8, None);
            ("3p4/4pppp/8/8/8/2r5/8/8", h8, c3, None);
            ("3p4/4pppp/8/8/8/2r5/8/8", d7, c3, Some 0);
            ("8/1ppppppp/p7/8/8/8/8/7R", e2, h1, None);
            ("8/1ppppppp/8/p7/8/8/8/7R", e2, h1, Some 4);
            ("8/8/8/8/6p1/4pp2/8/8", f2, c5, Some 2);
            ("8/8/8/8/6p1/4pp2/8/8", e2, c5, Some 1);
            ("8/8/8/8/6p1/4pp2/8/8", h2, c5, Some 0);
            ("8/8/8/8/6p1/4pp2/8/8", h2, c5, Some 0);
            ("8/5p2/6pp/7p/5pp1/4pp2/8/8", f2, c5, Some 5);
            ("2p5/ppp5/8/8/8/8/8/8", a8, b8, Some 0);
            ("2p5/ppp5/8/8/8/8/8/8", a8, e8, None);
            ("2p5/ppp5/8/8/8/8/8/8", a1, a8, None);
            ("8/1ppppppp/p5p1/8/8/p1p5/3p4/8", e2, h8, Some 4);
            ("8/1ppppppp/p5p1/8/8/p1p5/3p4/8", g1, h8, None);
            ("8/1ppppppp/p5p1/8/8/p1p5/3p4/8", g1, g8, Some 0);
            ("8/1ppppppp/p5p1/8/8/p1p5/3p4/8", b1, g8, None);
            ("8/1ppppppp/p5p1/8/8/p1p5/3p4/8", a1, g8, None);
            ("8/1ppppppp/p5p1/8/8/p1p5/3p4/8", d1, g8, Some 0);
          ]

    let tests =
      Alcotest.
        [
          test_case "pawn_candidate_origins" `Quick test_pawn_candidate_origins;
          test_case "k_groups" `Quick test_k_groups;
          test_case "path_from_origin" `Quick test_path_from_origin;
        ]
  end

  module TestRules = struct
    let test_static_rule () =
      List.iter
        (fun (fen, expected_static, expected_non_static) ->
          let pos = Position.of_fen (fen ^ " - 0 1") in
          let state = Rules.(apply (State.init pos) [ static_rule ]) in
          let is_static s = SquareSet.mem s state.static in
          assert (List.for_all is_static expected_static);
          assert (List.for_all (Fun.negate is_static) expected_non_static))
        [
          ( "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq",
            [ a1; c1; e1; f1; h1; d2; a8; c8; d8; e8; f8; h8; e7 ],
            [ b1; g1; b8; g8 ] );
          ( "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w -",
            [ d1; e1; d8; e8 ],
            [] );
          ( "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq",
            [ a1; c1; e1; h1; d2; a8; c8; d8; e8; f8; h8 ],
            [ d1; f1; e2 ] );
          ("4k3/8/8/8/8/8/1P1P4/2B1K3 w K", [ c1; b2; d2 ], [ e1 ]);
        ]

    let test_material_rule () =
      List.iter
        (fun (fen, legality) ->
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let state = Rules.(apply (State.init pos) [ material_rule ]) in
          legality_assertion state legality)
        [
          ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR", TBD);
          ("rnbqkbnr/pppppppp/8/8/8/P7/PPPPPPPP/1NBQKBNR", Illegal);
          ("rnbqkbnr/pppppppp/8/8/8/B7/PPPPPPPP/RNBQKBNR", Illegal);
          ("rnbqkbnr/pppppppp/8/8/8/B7/PPPPPPPP/RN1QKBNR", Illegal);
          ("rqrqkb1r/p1b4p/p6p/p6p/8/8/8/4K3", Illegal);
          ("rqr1kb1r/p1b4p/p6p/p6p/8/8/8/4K3", TBD);
        ]

    let test_origins_rule () =
      List.iter
        (fun (fen, expected_origins, legality) ->
          let rules =
            Rules.[ origins_rule; refine_origins_rule; static_rule ]
          in
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let state = Rules.(apply (State.init pos) rules) in
          legality_assertion state legality;
          let exists_pair (s, t) =
            SquareMap.find_opt s state.origins = Some (SquareSet.singleton t)
          in
          assert (List.for_all exists_pair expected_origins))
        [
          ( "r2q1rk1/1p1pp2p/pp5p/7p/3P4/7P/P1PPPP1P/R2Q1R1K",
            [ (h3, g2); (b6, c7); (h5, f7); (d4, b2); (g8, e8); (h1, e1) ],
            TBD );
          ( "r2qk2r/pppppp2/1B6/4P3/4P3/1P2PB2/P1PPP3/RN1QK1NR",
            [ (e5, h2); (e4, g2); (e3, f2); (b3, b2); (f3, f1); (b6, c1) ],
            TBD );
          ("4k3/P6p/P6p/P6p/P6p/P6p/P6p/4K3", [ (h2, c7); (a7, f2) ], TBD);
          ("4k3/4p2p/5pp1/8/6p1/8/8/4K3", [ (g4, d7) ], TBD);
          ("7k/8/P7/P7/PP6/2P5/8/7K", [ (a6, e2) ], TBD);
          ("4k3/4p2p/5pp1/6p1/8/8/8/4K3", [], Illegal);
          ("4k3/4p2p/5p1p/6p1/8/8/8/4K3", [], Illegal);
          ("rnbqkbnr/pppppppp/8/8/8/P7/PPPPPPPP/1NBQKBNR", [], Illegal);
          ("rnbqkbnr/pppppppp/8/8/8/B7/PPPPPPPP/RNBQKBNR", [], Illegal);
          ("rqrqkb1r/p1b4p/p6p/p6p/8/8/8/4K3", [], Illegal);
          ("rqr1kb1r/p1b4p/p6p/p6p/8/8/8/4K3", [], TBD);
        ]

    let test_destinies_rule () =
      List.iter
        (fun (fen, expected_destinies, not_expected_destinies) ->
          let pos = Position.of_fen (fen ^ " ? 1") in
          let state = Rules.(apply (State.init pos) Rules.all_rules) in
          let destinies_match (s, ts) =
            match SquareMap.find_opt s state.destinies with
            | None -> false
            | Some set -> SquareSet.(equal set (of_list ts))
          in
          let not_found (s, t) =
            not @@ SquareSet.mem t (SquareMap.find s state.destinies)
          in
          assert (List.for_all destinies_match expected_destinies);
          assert (List.for_all not_found not_expected_destinies))
        [
          ( "r2qk2r/pppppp2/1B6/4P3/4P3/1P2PB2/PRPPP3/1N1QK1NR w K -",
            [ (a1, [ b2 ]); (c1, [ b6 ]); (c8, [ c8 ]) ],
            [ (f8, b3); (f8, d2) ] );
          ( "rnbqkbnr/pppppppp/8/8/8/P1PP4/1P2PPPP/1N1QKBNR w Kkq -",
            [ (a1, [ a1; a2; b1; c1; c2; d1; d2 ]) ],
            [] );
        ]

    let test_static_mobility_rule () =
      List.iter
        (fun (fen, s, reachable, unreachable) ->
          let connected g s t =
            Option.is_some @@ Mobility.path ~to_avoid:SquareSet.empty g s t
          in
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let bP_in_s s = Position.piece_at s pos = Some Piece.bP in
          let static = List.filter bP_in_s Board.squares in
          let state = State.init pos in
          let state = { state with static = SquareSet.of_list static } in
          let state = Rules.(apply state [ static_mobility_rule ]) in
          let p = Position.piece_at s pos |> Option.get in
          let g = PieceMap.find p state.mobility in
          assert (List.for_all (connected g s) reachable);
          assert (not @@ List.exists (connected g s) unreachable))
        [
          ( "4k3/pppppppp/8/8/8/8/8/8",
            e8,
            [ a8; d8; f8; g8; h8 ],
            [ a7; h2; e5 ] );
          ( "4p3/p1pppppp/1p6/4pp2/4p1p1/4p2p/ppppppp1/3Q4",
            d1,
            [ a8; d8; a3 ],
            [ f8; h8 ] );
          ("8/3p4/2p1p3/1p2Rp2/2p1p3/2p2p2/3pp3/8", e5, [ c5; e3 ], [ a8; h1 ]);
          ( "8/3p1p2/2p5/1p3p2/p5p1/8/8/7B",
            h1,
            [ b1; d1; h1; d5; e6 ],
            [ c8; a6; b5; g4; g8; h7 ] );
          ( "8/8/8/6p1/4ppp1/3p1pp1/4p3/3p2N1",
            g1,
            [ g1; h3; f2; h1 ],
            [ d1; c2; a8; a7; g8; h8; d5 ] );
          ("8/8/3p2p1/6p1/3p2p1/8/5P2/8", f2, [ d5; a8; h5 ], [ c5; a7; h3 ]);
        ]

    let test_static_king_rule () =
      let open Piece in
      let open Square in
      let rules = Rules.[ static_rule; static_king_rule ] in
      let pos = Position.of_fen "r3k3/8/8/8/8/8/8/4K3 w q - 0 1" in
      let state = Rules.(apply (State.init pos) rules) in
      let missing_edge (p, s, t) =
        lacks_edge (PieceMap.find p state.mobility) (s, t)
      in
      let wP_not = [ (wP, d7, d8); (wP, d7, c8); (wP, f7, f8); (wP, f7, g8) ] in
      let wP_yes = [ (wP, e7, d8); (wP, e7, f8); (wP, e6, d7); (wP, f6, f7) ] in
      let wN_not = [ (wN, f6, h5); (wN, d6, c8); (wN, c7, a8); (wN, g7, f5) ] in
      let wN_yes = [ (wN, h5, f6); (wN, c8, d6); (wN, f3, h2); (wN, c2, a3) ] in
      let wB_not = [ (wB, d7, c8); (wB, d7, e6); (wB, f7, g8); (wB, f7, h5) ] in
      let wB_yes = [ (wB, c8, d7); (wB, e6, d7); (wB, g8, f7); (wB, f2, g3) ] in
      let wR_not = [ (wR, e7, f7); (wR, f8, h8); (wR, f8, f1); (wR, d8, d6) ] in
      let wR_yes = [ (wR, f7, e7); (wR, h8, f8); (wR, f1, f8); (wR, d1, d8) ] in
      let wQ_not = [ (wQ, d7, a7); (wQ, d8, e7); (wQ, f8, f6); (wQ, f7, f1) ] in
      let wQ_yes = [ (wQ, a7, d7); (wQ, d1, d8); (wQ, f1, f8); (wQ, f1, f7) ] in
      (* let wK_not = [ (wK, d7, d6); (wK, d8, c8); (wK, f6, f7); (wK, d6, e7) ] in *)
      (* let wK_yes = [ (wK, e2, e3); (wK, e1, e2); (wK, f5, g6); (wK, h8, g8) ] in *)
      let white_not = wP_not @ wN_not @ wB_not @ wR_not @ wQ_not in
      let white_yes = wP_yes @ wN_yes @ wB_yes @ wR_yes @ wQ_yes in
      (* assert (Mobility.G.is_empty @@ PieceMap.find bK state.mobility); *)
      assert (List.for_all missing_edge white_not);
      assert (not @@ List.exists missing_edge white_yes)

    let test_pawn_on_3rd_rank_rule () =
      let rules =
        Rules.[ origins_rule; refine_origins_rule; pawn_on_3rd_rank_rule ]
      in
      List.iter
        (fun (fen, c, connections) ->
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let state = Rules.(apply (State.init pos) rules) in
          PieceMap.iter
            (fun p g ->
              if p <> Piece.cP c then
                assert (List.for_all (lacks_edge g) connections))
            state.mobility)
        Square.
          [
            ( "4k3/8/8/8/8/1P3P2/P1PPPP1P/4K3",
              Color.white,
              [ (b1, b5); (b2, b3); (b8, b2); (h1, f3); (f3, g2); (a8, h1) ] );
            ( "4k3/pp1pppp1/3p3p/8/8/8/8/4K3",
              Color.black,
              [ (b8, f4); (c7, d6); (d6, c7); (h2, b8); (h3, h8); (h7, h6) ] );
          ]

    let test_route_from_origin_rule () =
      let rules =
        Rules.
          [
            static_rule;
            origins_rule;
            refine_origins_rule;
            static_mobility_rule;
            route_from_origin_rule;
            captures_rule;
          ]
      in
      List.iter
        (fun (fen, expected_origins, legality) ->
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let state = Rules.(apply (State.init pos) rules) in
          legality_assertion state legality;
          let exists_pair (s, t) =
            SquareMap.find_opt s state.origins = Some (SquareSet.singleton t)
          in
          assert (List.for_all exists_pair expected_origins))
        [
          ("rnbqkbnr/pppppppp/8/8/8/B7/PPPPPPPP/RN1QKBNR", [], Illegal);
          ("rnbqkbnr/pppppppp/8/8/8/B7/PPPPPP1P/RN1QKBNR", [], Illegal);
          ("r3kbnr/pppppp1p/6p1/8/8/B7/PPPPPP1P/RN1QKBNR", [ (a3, g2) ], TBD);
          ("rnbqkbnr/1ppppppp/p7/8/8/B7/PPPPPP1P/RN1QKBNR", [], Illegal);
          ("r3k2r/1ppppppp/p7/8/8/B7/PPPPP1PP/RN1QKBNR", [], Illegal);
          ("4k2r/1ppppppp/p7/8/8/B7/PPPPP1PP/RN1QKBNR", [ (a3, f2) ], TBD);
        ]

    let test_captures_rule () =
      let rules =
        Rules.
          [
            static_rule;
            origins_rule;
            refine_origins_rule;
            static_mobility_rule;
            route_from_origin_rule;
            destinies_rule;
            captures_rule;
          ]
      in
      List.iter
        (fun (fen, expected_captures) ->
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let state = Rules.(apply (State.init pos) rules) in
          List.iter
            (fun (s, n) -> assert (n = fst (SquareMap.find s state.captures)))
            expected_captures)
        [
          ("4k3/8/8/8/8/7P/7P/4K3", [ (g2, 1) ]);
          ("4k3/8/8/8/7P/7P/7P/4K3", [ (g2, 1); (f2, 2) ]);
          ("4k3/P7/P7/8/1P6/2P5/P2P4/4K3", [ (e2, 4); (f2, 5) ]);
          ("r3kb1r/1ppppppp/p7/8/8/B7/PP1PPPPP/RN1QKBNR", [ (c2, 3) ]);
          ("r3k3/1ppppppp/8/p6R/8/8/PPPPP1PP/1NBQKBNR", [ (f2, 5) ]);
          ("r1bqkb1r/1ppppppp/8/2P5/8/8/PPPPP1PP/R1BQKB1R", [ (f2, 3) ]);
        ]

    let test_too_many_captures_rule () =
      let rules =
        Rules.
          [
            static_rule;
            origins_rule;
            refine_origins_rule;
            static_mobility_rule;
            route_from_origin_rule;
            destinies_rule;
            captures_rule;
            too_many_captures_rule;
          ]
      in
      List.iter
        (fun (fen, legality) ->
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let state = Rules.(apply (State.init pos) rules) in
          legality_assertion state legality)
        [
          ("3qk3/3ppp2/8/P6P/P6P/P6P/P6P/4K3", Illegal);
          ("3qk3/3pp3/8/P6P/P6P/P6P/P6P/4K3", TBD);
          ("2bqk1B1/ppppppp1/7p/8/P7/5P1P/P3P1P1/4K3", Illegal);
          ("3qk1B1/ppppppp1/7p/8/P7/5P1P/P3P1P1/4K3", TBD);
        ]

    let test_missing_rule () =
      let open Square in
      List.iter
        (fun (fen, expected_definites, expected_candidates, not_missing) ->
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let rules =
            Rules.
              [
                static_rule;
                origins_rule;
                refine_origins_rule;
                static_mobility_rule;
                route_from_origin_rule;
                missing_rule;
              ]
          in
          let state = Rules.(apply (State.init pos) rules) in
          let white = ColorMap.find Color.White state.missing in
          let black = ColorMap.find Color.Black state.missing in
          let definite = SquareSet.union white.definite black.definite in
          let candidates = SquareSet.union white.candidates black.candidates in
          let is_definite s = SquareSet.mem s definite in
          let is_candidate s = SquareSet.mem s candidates in
          assert (List.for_all is_definite expected_definites);
          assert (List.for_all is_candidate expected_candidates);
          assert (List.for_all (Fun.negate is_definite) not_missing);
          assert (List.for_all (Fun.negate is_candidate) not_missing))
        [
          ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR", [], [], Board.squares);
          ("r1bqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR", [], [ b8; g8 ], []);
          ("r1bqkb1r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR", [ b8; g8 ], [], []);
          ("4k3/8/8/8/8/8/PPPPPPPP/1N1QKBNR", [ a1; c1 ], [], [ e8; h1 ]);
          ("4k3/8/8/8/P7/1P6/2P5/4K3", [ d2; d1 ], [], [ a2; b2; c2 ]);
          ("4k3/8/8/8/P7/1PP5/8/4K3", [], [ a2; b2; c2 ], []);
          ("r3k2r/ppp2ppp/8/8/8/8/8/4K3", [], [ a8; d7; e7; h8 ], []);
          ("3qk3/8/8/P7/P7/2P5/1P6/4K3", [ e2 ], [], [ a2 ]);
          ("1n2k3/p1pppppp/2p5/8/6B1/2P5/1P1PPPPP/RNBQK1NR", [ f1 ], [], [ a2 ]);
          ( "r2qk1nr/pp2pppp/1pp5/8/5P2/6P1/PPPP2PP/R1BQK1NR",
            [ f8; c8; f1 ],
            [ b8; g8; b1; g1 ],
            [] );
        ]

    let test_tombs_rule () =
      List.iter
        (fun (fen, expected_white_tombs, expected_black_tombs) ->
          let rules =
            Rules.
              [
                static_rule;
                origins_rule;
                refine_origins_rule;
                knight_origins_rule;
                static_mobility_rule;
                static_king_rule;
                pawn_on_3rd_rank_rule;
                tombs_rule;
              ]
          in
          let state = Rules.apply (State.init @@ Position.of_fen fen) rules in
          let white_tombs = ColorMap.find Color.White state.tombs in
          let black_tombs = ColorMap.find Color.Black state.tombs in
          let equal_when_sorted l1 l2 =
            List.sort Square.compare l1 = List.sort Square.compare l2
          in
          assert (equal_when_sorted white_tombs expected_white_tombs);
          assert (equal_when_sorted black_tombs expected_black_tombs))
        [
          (* Black tombs need to be updated after we improve the logic *)
          ( "r1bqkb1r/1ppppppp/8/2P5/8/8/PPPPP1PP/R1BQKB1R w KQkq - ? 1",
            [ c5; d4; e3 ],
            [] );
          ( "rnbqk2r/pppppp1p/6p1/8/8/NN6/PPPPP1P1/RNBQKBNR w KQkq - ? 1",
            [ g7; g7 ],
            [] );
          ( "r1bqk1Br/pppppp1p/6p1/8/8/2B5/PPPPP1P1/RNBQKBNR w KQk - ? 1",
            [ g7; g7; f8 ],
            [] );
          ( "r2qkb1r/p1ppp1p1/1p3p1p/8/8/1B6/PPPPPPP1/RN1QKBNR w KQkq - ? 1",
            [ g6; h7; g8 ],
            [] );
          ( "rnbqkbnr/ppnppppp/8/8/8/8/PPPPPPP1/R3K3 w kq - ? 1",
            [],
            [ d6; e5; f4; g3; h2 ] );
          ( "r2qk2r/p1pppp1p/2p2p2/8/P6P/8/PP1PP1PP/R1BQKB1R w KQkq - ? 1",
            [ b3; a4; g3; h4 ],
            [ c6; f6 ] );
        ]

    let test_parity_rule () =
      List.iter
        (fun (fen, legality) ->
          let pos = Position.of_fen (fen ^ " ? 1") in
          let state = Rules.(apply (State.init pos) all_rules) in
          legality_assertion state legality)
        [
          ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq -", Illegal);
          ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -", TBD);
          ( "r1bqkb1r/pppppppp/2n2n2/8/8/P1N2N1P/1PPPPPP1/R1BQKB1R b KQkq -",
            Illegal );
          ("r1bqkb1r/pppppppp/2n2n2/8/8/P1N2N1P/1PPPPPP1/R1BQKB1R w KQkq -", TBD);
          ("rnbqkbnr/1ppppppp/p7/8/8/P4N1P/1PPPPPP1/RNBQKBR1 w - -", Illegal);
          ("rnbqkbnr/1ppppppp/p7/8/8/P4N1P/1PPPPPP1/RNBQKBR1 b - -", TBD);
          ("rnbqkbnr/1ppppppp/p7/8/8/P4N1P/1PPPPPP1/RNBQK2R b - -", TBD);
          ("rnbqkbnr/1ppppppp/p7/8/8/P4N1P/1PPPPPP1/RNBQK2R b K -", Illegal);
        ]

    let tests =
      Alcotest.
        [
          test_case "static_rule" `Quick test_static_rule;
          test_case "material_rule" `Quick test_material_rule;
          test_case "origins_rule" `Quick test_origins_rule;
          test_case "destinies_rule" `Quick test_destinies_rule;
          test_case "static_mobility_rule" `Quick test_static_mobility_rule;
          test_case "static_king_rule" `Quick test_static_king_rule;
          test_case "pawn_on_3rd_rank_rule" `Quick test_pawn_on_3rd_rank_rule;
          test_case "route_from_origin_rule" `Quick test_route_from_origin_rule;
          test_case "captures_rule" `Quick test_captures_rule;
          test_case "too_many_captures_rule" `Quick test_too_many_captures_rule;
          test_case "test_missing_rule" `Quick test_missing_rule;
          test_case "test_tombs_rule" `Quick test_tombs_rule;
          test_case "test_parity_rule" `Quick test_parity_rule;
        ]
  end

  module TestSynergies = struct
    let test_origins_with_captures_lower_bound () =
      (* The starting origin squares can be further refined if we have into
         account the number of missing pieces and the number of captures
         that each specific piece can affort. *)
      List.iter
        (fun (fen, expected_origins) ->
          let rules =
            Rules.
              [
                static_rule;
                origins_rule;
                refine_origins_rule;
                static_mobility_rule;
                route_from_origin_rule;
                destinies_rule;
                captures_rule;
              ]
          in
          let pos = Position.of_fen (fen ^ " w - - 0 1") in
          let state = Rules.(apply (State.init pos) rules) in
          let exists_pair (s, t) =
            SquareMap.find_opt s state.origins = Some (SquareSet.singleton t)
          in
          assert (List.for_all exists_pair expected_origins))
        [
          ("rnbqkbnr/pppppppp/8/8/8/1PP5/P2PPPPP/RNBQKBNR", [ (b3, b2) ]);
          ("rnbqkb1r/pppppppp/8/8/8/1PP4P/P2PPP1P/RNBQKBNR", [ (b3, b2) ]);
          ("r1bqkb1r/pppppppp/8/8/8/1PP2P1P/P2P1P1P/RNBQKBNR", [ (b3, b2) ]);
          ( "r2qk2r/p1pppp1p/1p2P1pP/8/8/6P1/1PPPPP2/RNBQKBNR",
            [ (g3, g2); (e6, a2) ] );
          ("rnbqkb1B/pppppp1p/6p1/8/P1P5/8/1P1PPPP1/RNBQKBNR", [ (a4, a2) ]);
          ("r2qkb1B/p1pppp1p/1p4p1/8/P1P5/4P3/1P1PP1P1/RNBQKBNR", [ (a4, a2) ]);
          ( "rnbqkbnr/pp5p/7p/2p4p/3p3p/8/1PPPPP2/1NBQKB2",
            [ (d4, d7); (c5, c7) ] );
        ]

    let tests =
      Alcotest.
        [
          test_case "test_origins_with_captures_lower_bound" `Quick
            test_origins_with_captures_lower_bound;
        ]
  end
end

let () =
  let open Alcotest in
  run "Legality.Internal"
    [
      ("Helpers", Internal.TestHelpers.tests);
      ("Rules", Internal.TestRules.tests);
      ("Synergies", Internal.TestSynergies.tests);
    ]
