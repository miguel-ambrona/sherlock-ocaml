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

    let tests =
      Alcotest.
        [
          test_case "pawn_candidate_origins" `Quick test_pawn_candidate_origins;
          test_case "k_groups" `Quick test_k_groups;
        ]
  end

  module TestRules = struct
    let test_static_rule () =
      List.iter
        (fun (fen, expected_static, expected_non_static) ->
          let pos = Position.of_fen fen in
          let state = Rules.(apply (State.init pos) [ static_rule ]) in
          let is_static s = SquareSet.mem s state.static in
          assert (List.for_all is_static expected_static);
          assert (List.for_all (Fun.negate is_static) expected_non_static))
        [
          ( "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            [ a1; c1; e1; f1; h1; d2; a8; c8; d8; e8; f8; h8; e7 ],
            [ b1; g1; b8; g8 ] );
          ( "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1",
            [ d1; e1; d8; e8 ],
            [] );
          ( "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1",
            [ a1; c1; e1; h1; d2; a8; c8; d8; e8; f8; h8 ],
            [ d1; f1; e2 ] );
          ("4k3/8/8/8/8/8/1P1P4/2B1K3 w K - 0 1", [ c1; b2; d2 ], [ e1 ]);
        ]

    let test_material_rule () =
      List.iter
        (fun (fen, legality) ->
          let pos = Position.of_fen fen in
          let state = Rules.(apply (State.init pos) [ material_rule ]) in
          legality_assertion state legality)
        [
          ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", TBD);
          ("rnbqkbnr/pppppppp/8/8/8/P7/PPPPPPPP/1NBQKBNR w KQkq - 0 1", Illegal);
          ("rnbqkbnr/pppppppp/8/8/8/B7/PPPPPPPP/RNBQKBNR w KQkq - 0 1", Illegal);
          ("rnbqkbnr/pppppppp/8/8/8/B7/PPPPPPPP/RN1QKBNR w KQkq - 0 1", Illegal);
          ("rqrqkb1r/p1b4p/p6p/p6p/8/8/8/4K3 w - - ? 1", Illegal);
          ("rqr1kb1r/p1b4p/p6p/p6p/8/8/8/4K3 w - - ? 1", TBD);
        ]

    let test_origins_rule () =
      List.iter
        (fun (fen, expected_origins, legality) ->
          let rules =
            Rules.[ origins_rule; refine_origins_rule; static_rule ]
          in
          let pos = Position.of_fen fen in
          let state = Rules.(apply (State.init pos) rules) in
          legality_assertion state legality;
          let exists_pair (s, t) =
            SquareMap.find_opt s state.origins = Some (SquareSet.singleton t)
          in
          assert (List.for_all exists_pair expected_origins))
        [
          ( "r2q1rk1/1p1pp2p/pp5p/7p/3P4/7P/P1PPPP1P/R2Q1R1K w - - 0 1",
            [ (h3, g2); (b6, c7); (h5, f7); (d4, b2); (g8, e8); (h1, e1) ],
            TBD );
          ( "r2qk2r/pppppp2/1B6/4P3/4P3/1P2PB2/P1PPP3/RN1QK1NR w KQkq - 0 1",
            [ (e5, h2); (e4, g2); (e3, f2); (b3, b2); (f3, f1); (b6, c1) ],
            TBD );
          ( "4k3/P6p/P6p/P6p/P6p/P6p/P6p/4K3 w - - 0 1",
            [ (h2, c7); (a7, f2) ],
            TBD );
          ("4k3/4p2p/5pp1/8/6p1/8/8/4K3 w - - 0 1", [ (g4, d7) ], TBD);
          ("7k/8/P7/P7/PP6/2P5/8/7K w - - 0 1", [ (a6, e2) ], TBD);
          ("4k3/4p2p/5pp1/6p1/8/8/8/4K3 w - - 0 1", [], Illegal);
          ("4k3/4p2p/5p1p/6p1/8/8/8/4K3 w - - 0 1", [], Illegal);
          ( "rnbqkbnr/pppppppp/8/8/8/P7/PPPPPPPP/1NBQKBNR w KQkq - 0 1",
            [],
            Illegal );
          ( "rnbqkbnr/pppppppp/8/8/8/B7/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            [],
            Illegal );
          ("rqrqkb1r/p1b4p/p6p/p6p/8/8/8/4K3 w - - ? 1", [], Illegal);
          ("rqr1kb1r/p1b4p/p6p/p6p/8/8/8/4K3 w - - ? 1", [], TBD);
        ]

    let tests =
      Alcotest.
        [
          test_case "static_rule" `Quick test_static_rule;
          test_case "material_rule" `Quick test_material_rule;
          test_case "origins_rule" `Quick test_origins_rule;
        ]
  end
end

let () =
  let open Alcotest in
  run "Legality.Internal"
    [
      ("Helpers", Internal.TestHelpers.tests);
      ("Rules", Internal.TestRules.tests);
    ]
