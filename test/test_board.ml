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

let invalid_argument (f : 'a -> 'b) (x : 'a) =
  try
    ignore @@ f x;
    false
  with Invalid_argument _ -> true

module Test_File = struct
  open Board
  open File

  let test_equal () =
    assert (equal a a);
    assert (equal h h);
    assert (not @@ equal b c)

  let test_of_char () =
    assert (equal a @@ of_char 'a');
    assert (equal b @@ of_char 'b');
    assert (equal c @@ of_char 'c');
    assert (equal d @@ of_char 'd');
    assert (equal e @@ of_char 'e');
    assert (equal f @@ of_char 'f');
    assert (equal g @@ of_char 'g');
    assert (equal h @@ of_char 'h');
    assert (invalid_argument of_char 'A');
    assert (invalid_argument of_char 'i')

  let test_to_char () =
    assert ('a' = to_char a);
    assert ('b' = to_char b);
    assert ('c' = to_char c);
    assert ('d' = to_char d);
    assert ('e' = to_char e);
    assert ('f' = to_char f);
    assert ('g' = to_char g);
    assert ('h' = to_char h)

  let tests =
    Alcotest.
      ( "File",
        [
          test_case "equal" `Quick test_equal;
          test_case "of_char" `Quick test_of_char;
          test_case "to_char" `Quick test_to_char;
        ] )
end

module Test_Rank = struct
  open Board
  open Rank

  let rank1 = of_char '1'
  let rank2 = of_char '2'
  let rank3 = of_char '3'
  let rank4 = of_char '4'
  let rank5 = of_char '5'
  let rank6 = of_char '6'
  let rank7 = of_char '7'
  let rank8 = of_char '8'

  let test_equal () =
    assert (equal rank1 rank1);
    assert (equal rank3 rank3);
    assert (not @@ equal rank7 rank8)

  let test_relative () =
    assert (equal rank1 (relative 1 Color.white));
    assert (equal rank2 (relative 2 Color.white));
    assert (equal rank3 (relative 3 Color.white));
    assert (equal rank4 (relative 4 Color.white));
    assert (equal rank5 (relative 5 Color.white));
    assert (equal rank6 (relative 6 Color.white));
    assert (equal rank7 (relative 7 Color.white));
    assert (equal rank8 (relative 8 Color.white));
    assert (equal rank1 (relative 8 Color.black));
    assert (equal rank2 (relative 7 Color.black));
    assert (equal rank3 (relative 6 Color.black));
    assert (equal rank4 (relative 5 Color.black));
    assert (equal rank5 (relative 4 Color.black));
    assert (equal rank6 (relative 3 Color.black));
    assert (equal rank7 (relative 2 Color.black));
    assert (equal rank8 (relative 1 Color.black))

  let test_of_char () =
    assert (equal rank1 @@ of_char '1');
    assert (equal rank2 @@ of_char '2');
    assert (equal rank3 @@ of_char '3');
    assert (equal rank4 @@ of_char '4');
    assert (equal rank5 @@ of_char '5');
    assert (equal rank6 @@ of_char '6');
    assert (equal rank7 @@ of_char '7');
    assert (equal rank8 @@ of_char '8');
    assert (invalid_argument of_char '0');
    assert (invalid_argument of_char '9')

  let test_to_char () =
    assert ('1' = to_char rank1);
    assert ('2' = to_char rank2);
    assert ('3' = to_char rank3);
    assert ('4' = to_char rank4);
    assert ('5' = to_char rank5);
    assert ('6' = to_char rank6);
    assert ('7' = to_char rank7);
    assert ('8' = to_char rank8)

  let tests =
    Alcotest.
      ( "Rank",
        [
          test_case "equal" `Quick test_equal;
          test_case "relative" `Quick test_relative;
          test_case "of_char" `Quick test_of_char;
          test_case "to_char" `Quick test_to_char;
        ] )
end

module Test_Square = struct
  open Board
  open Square

  let files = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
  let ranks = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ]
  let square_names = List.map (fun r -> List.map (fun f -> f ^ r) files) ranks
  let squares = List.map (List.map of_string) square_names

  (* squares sorted as A1-H1, H2-A2, A3-H3, ..., H8-A8 *)
  let sorted_squares =
    List.mapi (fun i l -> if i mod 2 = 1 then List.rev l else l) squares
    |> List.concat

  let white_squares = List.filteri (fun i _ -> i mod 2 = 1) sorted_squares
  let black_squares = List.filteri (fun i _ -> i mod 2 = 0) sorted_squares

  let test_color () =
    assert (List.for_all (fun s -> Color.is_white (color s)) white_squares);
    assert (List.for_all (fun s -> Color.is_black (color s)) black_squares)

  let test_file () =
    assert (File.equal (file a1) (File.of_char 'a'));
    assert (File.equal (file a2) (File.of_char 'a'));
    assert (File.equal (file f5) (File.of_char 'f'));
    assert (File.equal (file h8) (File.of_char 'h'))

  let test_rank () =
    assert (Rank.equal (rank a1) (Rank.of_char '1'));
    assert (Rank.equal (rank a2) (Rank.of_char '2'));
    assert (Rank.equal (rank f5) (Rank.of_char '5'));
    assert (Rank.equal (rank h8) (Rank.of_char '8'))

  let test_equal () =
    assert (equal a1 a1);
    assert (equal a2 a2);
    assert (not @@ equal a1 a2)

  let test_of_string () =
    assert (
      List.for_all
        (fun s -> Square.equal s @@ of_string (to_string s))
        (List.concat squares))

  let test_to_string () =
    assert (
      List.for_all
        (fun str -> String.equal str @@ to_string (of_string str))
        (List.concat square_names))

  let test_in_relative_rank () =
    assert (in_relative_rank 1 Color.white a1);
    assert (in_relative_rank 8 Color.black a1);
    assert (in_relative_rank 2 Color.white a2);
    assert (in_relative_rank 7 Color.black a2);
    assert (in_relative_rank 5 Color.white f5);
    assert (in_relative_rank 4 Color.black f5);
    assert (in_relative_rank 8 Color.white h8);
    assert (in_relative_rank 1 Color.black h8);
    assert (not @@ in_relative_rank 1 Color.black a1);
    assert (not @@ in_relative_rank 2 Color.black a2);
    assert (not @@ in_relative_rank 5 Color.black f5);
    assert (not @@ in_relative_rank 8 Color.black h8);
    assert (not @@ in_relative_rank 8 Color.white a1)

  let test_king_distance () =
    assert (king_distance a1 a1 = 0);
    assert (king_distance a1 a2 = 1);
    assert (king_distance a1 f5 = 5);
    assert (king_distance a1 h8 = 7);
    assert (king_distance a2 h8 = 7);
    assert (king_distance f5 h8 = 3)

  let tests =
    Alcotest.
      ( "Square",
        [
          test_case "color" `Quick test_color;
          test_case "file" `Quick test_file;
          test_case "rank" `Quick test_rank;
          test_case "equal" `Quick test_equal;
          test_case "of_string" `Quick test_of_string;
          test_case "to_string" `Quick test_to_string;
          test_case "in_relative_rank" `Quick test_in_relative_rank;
          test_case "king_distance" `Quick test_king_distance;
        ] )
end

module Test_Piece = struct
  open Board
  open Piece

  let test_color () =
    let white_pieces = [ wK; wQ; wR; wB; wN; wP ] in
    let black_pieces = [ bK; bQ; bR; bB; bN; bP ] in
    assert (List.for_all (fun p -> Color.is_white @@ color p) white_pieces);
    assert (List.for_all (fun p -> Color.is_black @@ color p) black_pieces)

  let test_piece_type () =
    assert (List.for_all (fun p -> piece_type p = king) [ wK; bK ]);
    assert (List.for_all (fun p -> piece_type p = queen) [ wQ; bQ ]);
    assert (List.for_all (fun p -> piece_type p = rook) [ wR; bR ]);
    assert (List.for_all (fun p -> piece_type p = bishop) [ wB; bB ]);
    assert (List.for_all (fun p -> piece_type p = knight) [ wN; bN ]);
    assert (List.for_all (fun p -> piece_type p = pawn) [ wP; bP ])

  let test_equal () =
    assert (equal wK wK);
    assert (equal wP wP);
    assert (not @@ equal wP bP);
    assert (not @@ equal wN wB)

  let pts =
    [
      (king, 'k');
      (queen, 'q');
      (rook, 'r');
      (bishop, 'b');
      (knight, 'n');
      (pawn, 'p');
    ]

  let test_piece_type_of_char () =
    assert (List.for_all (fun (pt, c) -> pt = piece_type_of_char c) pts);
    assert (invalid_argument piece_type_of_char 'd')

  let test_piece_type_to_char () =
    assert (List.for_all (fun (pt, c) -> pt = piece_type_of_char c) pts)

  let ws = [ (wK, 'K'); (wQ, 'Q'); (wR, 'R'); (wB, 'B'); (wN, 'N'); (wP, 'P') ]
  let bs = [ (bK, 'k'); (bQ, 'q'); (bR, 'r'); (bB, 'b'); (bN, 'n'); (bP, 'p') ]
  let pieces = ws @ bs

  let test_of_char () =
    assert (List.for_all (fun (p, c) -> equal p (of_char c)) pieces);
    assert (invalid_argument piece_type_of_char 'd')

  let test_to_char () =
    assert (List.for_all (fun (p, c) -> Char.equal c (to_char p)) pieces)

  let tests =
    Alcotest.
      ( "Piece",
        [
          test_case "color" `Quick test_color;
          test_case "piece_type" `Quick test_piece_type;
          test_case "equal" `Quick test_equal;
          test_case "piece_type_of_char" `Quick test_piece_type_of_char;
          test_case "piece_type_to_char" `Quick test_piece_type_to_char;
          test_case "of_char" `Quick test_of_char;
          test_case "to_char" `Quick test_to_char;
        ] )
end

module Test_Direction = struct
  open Board
  open Direction
  open Square

  let test_limits () =
    assert (Option.is_none @@ north c8);
    assert (Option.is_none @@ south a1);
    assert (Option.is_none @@ south b1);
    assert (Option.is_none @@ west a1);
    assert (Option.is_none @@ south_west b1);
    assert (Option.is_none @@ east h8);
    assert (Option.is_some @@ east a1);
    assert (Option.is_some @@ west b1)

  let test_composition () =
    List.iter
      (fun s ->
        assert (north_west s = Option.bind (north s) west);
        assert (north_west s = Option.bind (west s) north);
        assert (north_east s = Option.bind (north s) east);
        assert (north_east s = Option.bind (east s) north);
        assert (south_west s = Option.bind (south s) west);
        assert (south_west s = Option.bind (west s) south);
        assert (south_east s = Option.bind (south s) east);
        assert (south_east s = Option.bind (east s) south))
      squares

  let test_opposite_directions () =
    let non_border_squares =
      List.filter
        (fun s ->
          file s <> File.a
          && file s <> File.h
          && rank s <> rank1
          && rank s <> rank8)
        squares
    in
    List.iter
      (fun s ->
        assert (Option.bind (north s) south = Some s);
        assert (Option.bind (south s) north = Some s);
        assert (Option.bind (east s) west = Some s);
        assert (Option.bind (west s) east = Some s);
        assert (Option.bind (north_east s) south_west = Some s);
        assert (Option.bind (south_west s) north_east = Some s);
        assert (Option.bind (north_west s) south_east = Some s);
        assert (Option.bind (south_east s) north_west = Some s))
      non_border_squares

  let test_neighbors () =
    assert (diag_neighbors e4 = [ f5; d5; f3; d3 ]);
    assert (straight_neighbors e4 = [ e5; e3; f4; d4 ]);
    assert (knight_neighbors e4 = [ f6; g5; d6; c5; f2; g3; d2; c3 ])

  let tests =
    Alcotest.
      ( "Direction",
        [
          test_case "limits" `Quick test_limits;
          test_case "composition" `Quick test_composition;
          test_case "opposite_directions" `Quick test_opposite_directions;
          test_case "neighbors" `Quick test_neighbors;
        ] )
end

module Test_Board = struct
  open Board

  let fens =
    [
      "5r1k/6P1/7K/5q2/8/8/8/8";
      "Bb1k1b2/bKp1p1p1/1pP1P1P1/1P6/p5P1/P7/8/8";
      "k7/8/2K5/8/8/8/8/8";
    ]

  let boards = List.map of_fen fens

  let test_equal () =
    assert (equal initial initial);
    assert (equal empty empty);
    assert (not @@ equal initial empty)

  let test_empty () = assert (equal empty (of_fen "8/8/8/8/8/8/8/8"))

  let test_initial () =
    assert (equal initial (of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"))

  let test_squares () = assert (List.length squares = 64)

  let test_of_fen () =
    assert (List.for_all (fun b -> equal b (of_fen (to_fen b))) boards)

  let test_to_fen () =
    assert (
      List.for_all (fun str -> String.equal str (to_fen (of_fen str))) fens)

  let test_piece_at () =
    let board = List.hd boards in
    assert (Some Piece.bR = piece_at Square.f8 board);
    assert (Some Piece.bK = piece_at Square.h8 board);
    assert (Some Piece.wP = piece_at Square.g7 board);
    assert (Some Piece.wK = piece_at Square.h6 board);
    assert (Some Piece.bQ = piece_at Square.f5 board);
    assert (Option.is_none @@ piece_at Square.g5 board);
    assert (Option.is_none @@ piece_at Square.a1 board)

  let test_set_piece () =
    let board = set_piece (Piece.bK, Square.a8) empty in
    let board = set_piece (Piece.wK, Square.c6) board in
    assert (equal board @@ List.nth boards 2)

  let test_remove_piece () =
    let board = remove_piece Square.c6 (List.nth boards 2) in
    let board = remove_piece Square.a8 board in
    assert (equal board empty);
    let board = List.hd boards in
    assert (equal board (remove_piece Square.a1 board))

  let test_of_pieces () =
    let pieces =
      Piece.[ (bR, "f8"); (bK, "h8"); (wP, "g7"); (wK, "h6"); (bQ, "f5") ]
      |> List.map (fun (p, str) -> (p, Square.of_string str))
    in
    assert (equal (List.hd boards) (of_pieces pieces))

  let test_to_pieces () =
    List.iter2
      (fun n board -> assert (n = List.length @@ to_pieces board))
      [ 5; 17; 2 ] boards

  let tests =
    Alcotest.
      ( "Board",
        [
          test_case "equal" `Quick test_equal;
          test_case "empty" `Quick test_empty;
          test_case "initial" `Quick test_initial;
          test_case "squares" `Quick test_squares;
          test_case "of_fen" `Quick test_of_fen;
          test_case "to_fen" `Quick test_to_fen;
          test_case "piece_at" `Quick test_piece_at;
          test_case "set_piece" `Quick test_set_piece;
          test_case "remove_piece" `Quick test_remove_piece;
          test_case "of_pieces" `Quick test_of_pieces;
          test_case "to_pieces" `Quick test_to_pieces;
        ] )
end

let () =
  let open Alcotest in
  run "Board"
    [
      Test_File.tests;
      Test_Rank.tests;
      Test_Square.tests;
      Test_Piece.tests;
      Test_Direction.tests;
      Test_Board.tests;
    ]
