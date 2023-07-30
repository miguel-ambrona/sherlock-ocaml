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

open Chess

module Test_Position = struct
  open Position

  let test_initial () =
    assert (
      equal initial
      @@ of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  let test_parsing () =
    let check (fen, expected) = assert (expected = to_fen (of_fen fen)) in
    List.iter check
      [
        (* Normal parsing *)
        ( "r1b1kb1r/6pp/p1pppn2/4P1B1/8/q1N5/P1PQ2PP/1R2KB1R b Kkq - 0 13",
          "r1b1kb1r/6pp/p1pppn2/4P1B1/8/q1N5/P1PQ2PP/1R2KB1R b Kkq - 0 13" );
        (* En passant rights *)
        ( "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
          "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1" );
        ( "rnbqkbnr/ppp2ppp/4p3/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3",
          "rnbqkbnr/ppp2ppp/4p3/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3" );
        ( "rnbqkbnr/ppp2ppp/4p3/3pP3/3P4/8/PPP2PPP/RNBQKBNR b KQkq e4 0 3",
          "rnbqkbnr/ppp2ppp/4p3/3pP3/3P4/8/PPP2PPP/RNBQKBNR b KQkq - 0 3" );
        (* Castling rights *)
        ( "r3k1r1/8/8/8/8/8/8/1R2K2R w KQkq - 0 1",
          "r3k1r1/8/8/8/8/8/8/1R2K2R w Kq - 0 1" );
        ( "r2k3r/8/8/8/8/8/8/R3K2R w KQkq - 0 1",
          "r2k3r/8/8/8/8/8/8/R3K2R w KQ - 0 1" );
        (* Halfmove counter *)
        ("8/8/8/8/8/8/8/8 w - - ? 1", "8/8/8/8/8/8/8/8 w - - ? 1");
        ("8/8/8/8/8/8/8/8 w - - -5 1", "8/8/8/8/8/8/8/8 w - - ? 1");
      ]

  let tests =
    Alcotest.
      ( "Position",
        [
          test_case "initial" `Quick test_initial;
          test_case "parsing" `Quick test_parsing;
        ] )
end

let () =
  let open Alcotest in
  run "Position" [ Test_Position.tests ]
