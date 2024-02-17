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
    assert (Option.is_some state.illegal)

  let test_which_color () =
    let rules = Condition.monochromatic :: Rules.all_rules in
    List.iter
      (fun (fen, illegal) ->
        let state = Rules.(apply (State.init @@ Position.of_fen fen) rules) in
        Debug.print_state state;
        assert (Option.is_some state.illegal = illegal))
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
    assert (Option.is_some state.illegal)

  let tests = Alcotest.[ test_case "static_king" `Quick test_static_king ]
end

let () =
  let open Alcotest in
  run "Compositions"
    [
      ("Smullyan", SmullyanCompositions.tests);
      ("Experimental", RuleSpecificCompositions.tests);
    ]
