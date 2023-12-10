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
open Legality

module Condition = struct
  let no_promotions (state : State.t) =
    let disable_8th_rank c g =
      let f _ t = not @@ Square.in_relative_rank 8 c t in
      Mobility.filter_edges f g
    in
    let mobility =
      PieceMap.mapi
        (fun p g ->
          if Piece.piece_type p <> Pawn then g
          else disable_8th_rank (Piece.color p) g)
        state.mobility
    in
    { state with mobility }

  let monochromatic (state : State.t) =
    let open Square in
    (* Enable 0-0 manually, since it is a monochromatic move. *)
    let add_short_castling p g =
      if p = Piece.wK then Mobility.G.add_edge_e g (e1, 0, g1)
      else if p = Piece.bK then Mobility.G.add_edge_e g (e8, 0, g8)
      else g
    in
    let disable_color_changes g =
      let f s t = Square.color s = Square.color t in
      Mobility.filter_edges f g
    in
    let mobility =
      state.mobility
      |> PieceMap.mapi add_short_castling
      |> PieceMap.map disable_color_changes
    in
    { state with mobility }
end

module SmullyanCompositions = struct
  let test_intro () =
    let fen = "4k3/5p2/2q5/8/8/6B1/1P1P1K2/8 w - - 0 1" in
    let rules = Condition.no_promotions :: Rules.all_rules in
    let state = Rules.(apply (State.init @@ Position.of_fen fen) rules) in
    assert state.illegal

  let test_which_color () =
    let rules = Condition.monochromatic :: Rules.all_rules in
    List.iter
      (fun (fen, illegal) ->
        let state = Rules.(apply (State.init @@ Position.of_fen fen) rules) in
        Debug.print_state state;
        assert (state.illegal = illegal))
      [
        ("4k3/8/8/8/1K6/6P1/3P1P2/8 w - - 0 1", true);
        ("4k3/8/8/8/1K6/6p1/3P1P2/8 w - - 0 1", false);
      ]

  let tests =
    Alcotest.
      [
        test_case "intro" `Quick test_intro;
        test_case "which_color" `Quick test_which_color;
      ]
end

module RuleSpecificCompositions = struct
  let test_static_king () =
    let fen = "r3k3/ppp1p1pp/8/8/8/8/8/R1R1K2R w KQq - 0 1" in
    let state = Rules.(apply (State.init @@ Position.of_fen fen) all_rules) in
    Debug.print_state state;
    assert state.illegal

  let test_parity () =
    (* let fen = "rnbqkb1r/pppppppp/8/8/8/4P3/PPPPP1PP/RNBQKBNR b KQkq - 0 1" in *)
    let _fen =
      "Nrb1kb1r/pp1ppppp/1p4n1/8/2q3n1/P4P2/1PPPP1PP/R1BQKB1R w KQkq - ? 1"
    in
    let _fen =
      "Nrb1kb1r/pp1ppp1p/1p3pn1/8/6n1/P6P/1PPPP1P1/RqBQKB1R b KQk - ? 1"
      (* This logic still needs to handle the illegality of this:
         Nrb1kb1r/pp1ppppp/1p3Nn1/8/6n1/P7/1PPPP1PP/RqBQKB1R b KQk - ? 0 true! *)
    in
    (* The next one run into trouble, knights appeared and kept being retracted *)
    (* RUNNING THIS NOW, UPDATE WHEN FINISHED! *)
    (* let fen = "2kr1b1r/npp1pppp/1pp5/1n6/8/P6b/1PPPPPP1/R1BQKB1R w KQ - ? 1" in *)
    (* The next one is fine, it understands it is not illegal, but it takes some time *)
    let _fen = "r3kb1r/npp1pppp/1pp5/1n6/8/P6b/1PPPPPP1/R1BQKB1R b KQq - ? 1" in
    (* We found that the following is illegal! But it takes some time *)
    (* let fen = "r1bqkb1r/1pppppp1/p6B/8/1N6/1PP5/NPP1PPPP/2KR1B1R w kq - ? 1" in *)
    let fen =
      "1Nbqkb1r/r1ppp1p1/p1p2p1p/8/8/8/PPPPPPPP/RnBQKBnR w KQk - ? 14"
    in
    let pos = Position.of_fen fen in
    assert (not @@ is_legal pos)

  let tests =
    Alcotest.
      [
        test_case "static_king" `Quick test_static_king;
        test_case "parity" `Quick test_parity;
      ]
end

let () =
  let open Alcotest in
  run "Compositions"
    [
      ("Smullyan", SmullyanCompositions.tests);
      ("Experimental", RuleSpecificCompositions.tests);
    ]
