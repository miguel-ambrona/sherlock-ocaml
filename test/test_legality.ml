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

  let empty_state fen =
    Rules.{ pos = Position.of_fen fen; events = EventSet.empty }

  let test_static_rule () =
    List.iter
      (fun (fen, expected_static, expected_non_static) ->
        let state = Rules.apply (empty_state fen) Rules.static_rule in
        let static s = EventSet.mem (Event.Static s) state.events in
        assert (List.for_all static expected_static);
        assert (List.for_all (Fun.negate static) expected_non_static))
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
      (fun (fen, invalid) ->
        let state = Rules.apply (empty_state fen) Rules.material_rule in
        Helpers.print_events state.events;
        assert (invalid = EventSet.mem Event.Contradiction state.events))
      [
        ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", false);
        ("rnbqkbnr/pppppppp/8/8/8/P7/PPPPPPPP/1NBQKBNR w KQkq - 0 1", true);
        ("rnbqkbnr/pppppppp/8/8/8/B7/PPPPPPPP/RNBQKBNR w KQkq - 0 1", true);
        ("rnbqkbnr/pppppppp/8/8/8/B7/PPPPPPPP/RN1QKBNR w KQkq - 0 1", true);
        ("rqrqkb1r/p1b4p/p6p/p6p/8/8/8/4K3 w - - ? 1", true);
        ("rqr1kb1r/p1b4p/p6p/p6p/8/8/8/4K3 w - - ? 1", false);
      ]

  let tests =
    Alcotest.
      [
        test_case "static_rule" `Quick test_static_rule;
        test_case "material_rule" `Quick test_material_rule;
      ]
end

let () =
  let open Alcotest in
  run "Legality" [ ("Legality.Internal", Internal.tests) ]
