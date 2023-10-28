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

module Test_Move = struct
  open Board
  open Square
  open Move

  let e2e4 = Move.{ source = e2; target = e4; promoted = None }
  let e7e5 = Move.{ source = e7; target = e5; promoted = None }
  let h7h8q = Move.{ source = h7; target = h8; promoted = Some Piece.queen }

  let test_equal () =
    assert (equal e2e4 e2e4);
    assert (not @@ equal e2e4 e7e5)

  let test_of_string () =
    assert (equal e2e4 @@ of_string "e2e4");
    assert (equal e7e5 @@ of_string "e7e5");
    assert (equal h7h8q @@ of_string "h7h8q")

  let test_to_string () =
    assert (String.equal "e2e4" @@ to_string e2e4);
    assert (String.equal "e7e5" @@ to_string e7e5);
    assert (String.equal "h7h8q" @@ to_string h7h8q)

  let rec perft depth pos =
    if depth <= 0 then 1
    else if depth = 1 then List.length (legal_moves pos)
    else
      let childs = List.map (apply pos) (legal_moves pos) in
      List.fold_left Int.add 0 @@ List.map (perft (depth - 1)) childs

  (* Test vectors taken from https://www.chessprogramming.org/Perft_Results *)
  let test_perft ~slow () =
    let positions =
      [
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 \
         10";
      ]
      |> List.map Position.of_fen
    in
    let expected =
      [
        [ 20; 400; 8_902; 197_281; 4_865_609 ];
        [ 48; 2_039; 97_862; 4_085_603 ];
        [ 14; 191; 2_812; 43_238; 674_624 ];
        [ 6; 264; 9467; 422_333 ];
        [ 44; 1_486; 62_379; 2_103_487 ];
        [ 46; 2_079; 89_890; 3_894_594 ];
      ]
    in
    List.iter2
      (fun pos ->
        List.iteri (fun i n ->
            if n > 500_000 && not slow then ()
            else assert (perft (i + 1) pos = n)))
      positions expected

  let tests =
    Alcotest.
      ( "Move",
        [
          test_case "equal" `Quick test_equal;
          test_case "of_string" `Quick test_of_string;
          test_case "to_string" `Quick test_to_string;
          test_case "perft" `Quick @@ test_perft ~slow:false;
        ] )
end

let () =
  let open Alcotest in
  run "Move" [ Test_Move.tests ]
