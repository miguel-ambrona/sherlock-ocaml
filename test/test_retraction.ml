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

module Test_Retraction = struct
  open Board
  open Square
  open Retraction

  let e2e4 =
    { source = e2; target = e4; retraction_type = Normal; uncaptured = None }

  let e7e5 =
    { source = e7; target = e5; retraction_type = Normal; uncaptured = None }

  let a1xNa8 =
    {
      source = a1;
      target = a8;
      retraction_type = Normal;
      uncaptured = Some Piece.knight;
    }

  let g7xQh8prom =
    {
      source = g7;
      target = h8;
      retraction_type = UnPromotion;
      uncaptured = Some Piece.queen;
    }

  let e5d6ep =
    {
      source = e5;
      target = d6;
      retraction_type = UnEnPassant;
      uncaptured = None;
    }

  let test_equal () =
    assert (equal e2e4 e2e4);
    assert (not @@ equal e2e4 e7e5)

  let test_of_string () =
    assert (equal e2e4 @@ of_string "e2e4");
    assert (equal e7e5 @@ of_string "e7e5");
    assert (equal a1xNa8 @@ of_string "a1xNa8");
    assert (equal g7xQh8prom @@ of_string "g7xQh8prom");
    assert (equal e5d6ep @@ of_string "e5d6ep")

  let test_to_string () =
    assert (String.equal "e2e4" @@ to_string e2e4);
    assert (String.equal "e7e5" @@ to_string e7e5);
    assert (String.equal "a1xNa8" @@ to_string a1xNa8);
    assert (String.equal "g7xQh8prom" @@ to_string g7xQh8prom);
    assert (String.equal "e5d6ep" @@ to_string e5d6ep)

  let test_retractions test_vectors =
    let module FenSet = Set.Make (String) in
    List.iter
      (fun (fen, must_exist, must_not_exist) ->
        let prev_fens =
          retracted @@ Position.of_fen fen
          |> List.map Position.to_fen |> List.to_seq |> FenSet.of_seq
        in
        List.iter (fun fen -> assert (FenSet.mem fen prev_fens)) must_exist;
        List.iter
          (fun fen -> assert (not @@ FenSet.mem fen prev_fens))
          must_not_exist)
      test_vectors

  let test_uncastling () =
    test_retractions
      [
        (* Test uncastling without halfmove-clock *)
        ( "5k2/8/8/8/8/8/8/5RK1 b - - ? 5",
          [
            "5k2/8/8/8/8/8/8/4R1K1 w - - ? 5";
            "5k2/8/8/8/8/8/8/1R4K1 w - - ? 5";
            "5k2/8/8/8/8/8/8/1R3qK1 w - - ? 5";
            "5k2/8/8/8/8/8/8/R5K1 w - - ? 5";
            "5k2/8/8/8/8/8/5K2/5R2 w - - ? 5";
            "5k2/8/8/8/8/8/8/4K2R w K - ? 5";
            "5k2/8/8/8/8/8/5K2/5Rb1 w - - ? 5";
          ],
          [ "5k2/8/8/8/8/8/8/4K2R w - - ? 5" ] );
        (* Test uncastling with halfmove-clock > 0 *)
        ( "5k2/8/8/8/8/8/8/5RK1 b - - 1 5",
          [
            "5k2/8/8/8/8/8/8/1R4K1 w - - 0 5";
            "5k2/8/8/8/8/8/8/R5K1 w - - 0 5";
            "5k2/8/8/8/8/8/5K2/5R2 w - - 0 5";
            "5k2/8/8/8/8/8/8/4K2R w K - 0 5";
          ],
          [
            "5k2/8/8/8/8/8/8/4K2R w - - 0 5";
            "5k2/8/8/8/8/8/8/1R3qK1 w - - 0 5";
            "5k2/8/8/8/8/8/5K2/5Rb1 w - - 0 5";
          ] );
        (* Uncastling is must not be a retraction if halfmove-clock = 0 *)
        ( "5k2/8/8/8/8/8/8/5RK1 b - - 0 5",
          [
            "5k2/8/8/8/8/8/8/1R3qK1 w - - ? 5";
            "5k2/8/8/8/8/8/5K2/5Rb1 w - - ? 5";
          ],
          [
            "5k2/8/8/8/8/8/8/4K2R w - - ? 5";
            "5k2/8/8/8/8/8/8/4R1K1 w - - ? 5";
            "5k2/8/8/8/8/8/5K2/5Rp1 w - - ? 5";
            "5k2/8/8/8/8/8/5K2/5RB1 w - - ? 5";
            "5k2/8/8/8/8/8/5R2/5nK1 w - - ? 5";
          ] );
        (* Uncastling with rooks on both sides *)
        ( "2kr3r/8/8/8/8/8/8/R3K2R w Q - ? 20",
          [
            "r3k2r/8/8/8/8/8/8/R3K2R b Qq - ? 19";
            "r3k2r/8/8/8/8/8/8/R3K2R b Qkq - ? 19";
          ],
          [
            "r3k2r/8/8/8/8/8/8/R3K2R b k - ? 19";
            "r3k2r/8/8/8/8/8/8/R3K2R b KQq - ? 19";
            "r3k2r/8/8/8/8/8/8/R3K2R b kq - ? 19";
          ] );
      ]

  let test_castling_rights () =
    test_retractions
      [
        (* Rooks or kings cannot retract if castling rights are set *)
        ( "5k1N/8/8/3P4/8/8/8/R3K3 b Q - 2 50",
          [ "5k2/5N2/8/3P4/8/8/8/R3K3 w Q - 1 50" ],
          [
            "5k1N/8/8/3P4/8/8/R7/4K3 w Q - 1 50";
            "5k1N/8/8/3P4/8/8/R7/4K3 w - - 1 50";
            "5k1N/8/8/3P4/8/8/5K2/R7 w Q - 1 50";
          ] );
      ]

  let test_unpromotion () =
    test_retractions
      [
        (* Test uncastling without halfmove-clock *)
        ( "2k5/8/8/8/8/8/8/2K2n2 w - - ? 10",
          [
            "2k5/8/8/8/8/8/5p2/2K5 b - - ? 9";
            "2k5/8/8/8/8/8/6p1/2K2Q2 b - - ? 9";
            "2k5/8/8/8/8/8/4p3/2K2B2 b - - ? 9";
          ],
          [
            "2k5/8/8/8/8/5p2/8/2K5 b - - ? 9";
            "2k5/8/8/8/8/8/4p3/2K5 b - - ? 9";
            "2k5/8/8/8/8/8/6p1/2K2q2 b - - ? 9";
          ] );
        (* Test uncastling with halfmove-clock > 0 *)
        ( "2k5/8/8/8/8/8/8/2K2n2 w - - 7 10",
          [ "2k5/8/8/8/8/6n1/8/2K5 b - - 6 9" ],
          [
            "2k5/8/8/8/8/8/5p2/2K5 b - - 6 9";
            "2k5/8/8/8/8/8/6p1/2K2Q2 b - - 6 9";
            "2k5/8/8/8/8/8/4p3/2K2B2 b - - 6 9";
          ] );
      ]

  let test_unenpassant () =
    test_retractions
      [
        ( "8/5k2/4P3/pP5p/8/8/8/4K3 b - - 0 7",
          [
            "8/5k2/8/pP2pP1p/8/8/8/4K3 w - e6 0 7";
            "8/5k2/8/pP1Pp2p/8/8/8/4K3 w - e6 0 7";
            "8/5k2/4p3/pP3P1p/8/8/8/4K3 w - a6 0 7";
            "8/5k2/4p3/pP3P1p/8/8/8/4K3 w - - ? 7";
          ],
          [
            "8/5k2/8/pP2pP1p/8/8/8/4K3 w - - ? 7";
            "8/5k2/8/pP1Pp2p/8/8/8/4K3 w - - ? 7";
            "8/5k2/4p3/pP3P1p/8/8/8/4K3 w - h6 0 7";
          ] );
      ]

  let test_double_unpush () =
    test_retractions
      [
        ( "4k3/8/8/3Pp3/8/8/8/4K3 w - e6 0 50",
          [ "4k3/4p3/8/3P4/8/8/8/4K3 b - - ? 49" ],
          [ "4k3/8/4p3/3P4/8/8/8/4K3 b - - ? 49" ] );
        ( "8/8/8/8/6P1/5N1p/5K1P/4N1Bk w - - ? 20",
          [ "8/8/8/8/6Pp/5N2/5K1P/4N1Bk b - g3 0 19" ],
          [
            "8/8/8/8/6P1/5N1p/5KpP/4N1BR b - - ? 19";
            "8/8/8/8/6Pp/5N2/5K1P/4N1Bk b - g3 ? 19";
          ] );
      ]

  let test_count () =
    List.iter
      (fun (fen, n) ->
        let prev_pos = retracted (Position.of_fen fen) in
        assert (List.length prev_pos = n))
      [
        ("8/4k3/8/KP4Pp/pP6/8/8/8 w - h6 5 10", 0);
        ("8/4k3/8/KP4Pp/pP6/8/8/8 w - h6 0 10", 2);
        ("4k3/8/8/3Pp3/8/8/8/4K3 w - e6 0 13", 1);
        ("4k3/8/8/3Pp3/8/8/8/4K3 w - e6 ? 13", 1);
        ("8/8/8/8/6P1/5N1p/5K1P/4N1Bk w - - ? 16", 2);
        ("8/8/8/p7/P5p1/4p1Pk/P3P2P/5bBK w - - ? 19", 35);
        ("8/5p2/5P2/8/1p6/kP2pp2/1pKpP3/3B4 w - - ? 30", 57);
        ("2k5/8/8/8/8/8/8/2K2n2 w - - ? 33", 54);
        ("5bk1/4p1p1/4P1P1/7K/8/8/8/8 w - - ? 36", 15);
        ("5b1k/4p1p1/4P1P1/7K/8/8/8/8 w - - ? 39", 10);
        ("k7/8/2K5/8/8/8/8/8 w - - 3 44", 2);
        ("k7/8/2K5/8/8/8/8/8 w - - 0 44", 8);
        ("k7/8/2K5/8/8/8/8/8 w - - ? 44", 10);
        ("5k1N/8/8/3P4/8/8/8/R3K3 b Q - 2 47", 1);
        ("Nr1bk1Br/1p2pp1p/1p1p1p2/pP6/6n1/8/1PPPP1P1/RqB1K2R w KQk a6 ? 50", 1);
        ("Nr1bk1Br/pp2pp1p/1p1p1p2/1P6/6n1/8/1PPPP1P1/RqB1K2R b KQk - ? 49", 12);
        ("8/6k1/7P/8/8/8/8/4K3 b - - 1 53", 0);
        ("8/6k1/7P/8/8/8/8/4K3 b - - 0 53", 7);
        ("8/6k1/7P/7P/8/8/8/4K3 b - - 0 53", 5);
        ("8/8/8/8/8/8/4PP1n/2k1K3 b - - 0 57", 4);
        ("8/8/8/4ppPp/5rpb/8/4PP1n/2k1K3 b - - 0 57", 12);
      ]

  let tests =
    Alcotest.
      ( "Retraction",
        [
          test_case "equal" `Quick test_equal;
          test_case "of_string" `Quick test_of_string;
          test_case "to_string" `Quick test_to_string;
          test_case "uncastling" `Quick @@ test_uncastling;
          test_case "castling_rights" `Quick @@ test_castling_rights;
          test_case "unpromotion" `Quick @@ test_unpromotion;
          test_case "unenpassant" `Quick @@ test_unenpassant;
          test_case "test_double_unpush" `Quick @@ test_double_unpush;
          test_case "count" `Quick @@ test_count;
        ] )
end

let () =
  let open Alcotest in
  run "Retraction" ~verbose:true [ Test_Retraction.tests ]
