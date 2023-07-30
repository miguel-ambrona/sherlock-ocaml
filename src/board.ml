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

module File = struct
  type t = int

  let equal = Int.equal
  let a, b, c, d, e, f, g, h = (0, 1, 2, 3, 4, 5, 6, 7)

  let validate t =
    if t < 0 || t > 7 then
      raise @@ Invalid_argument ("Unknown file: " ^ Char.escaped @@ Char.chr t)
    else t

  let of_char c = validate (Char.code c - 97)
  let to_char f = Char.chr (f + 97)
end

module Rank = struct
  type t = int

  let equal = Int.equal

  let validate t =
    if t < 0 || t > 7 then
      raise @@ Invalid_argument ("Unknown rank: " ^ Char.escaped @@ Char.chr t)
    else t

  let relative i c =
    if Color.is_white c then validate (i - 1) else validate (8 - i)

  let of_char c = validate (Char.code c - 49)
  let to_char r = Char.chr (r + 49)
end

module Square = struct
  type t = int

  let file s = s mod 8
  let rank s = s / 8
  let equal = Int.equal
  let compare = Int.compare
  let color s = if (rank s + file s) mod 2 = 1 then Color.white else Color.black
  let is_valid s = 0 <= s && s < 64
  let of_string str = File.of_char str.[0] + (8 * Rank.of_char str.[1])

  let to_string s =
    Char.escaped (File.to_char @@ file s) ^ Char.escaped (Rank.to_char @@ rank s)

  let a8, b8, c8, d8, e8, f8, g8, h8 = (56, 57, 58, 59, 60, 61, 62, 63)
  let a7, b7, c7, d7, e7, f7, g7, h7 = (48, 49, 50, 51, 52, 53, 54, 55)
  let a6, b6, c6, d6, e6, f6, g6, h6 = (40, 41, 42, 43, 44, 45, 46, 47)
  let a5, b5, c5, d5, e5, f5, g5, h5 = (32, 33, 34, 35, 36, 37, 38, 39)
  let a4, b4, c4, d4, e4, f4, g4, h4 = (24, 25, 26, 27, 28, 29, 30, 31)
  let a3, b3, c3, d3, e3, f3, g3, h3 = (16, 17, 18, 19, 20, 21, 22, 23)
  let a2, b2, c2, d2, e2, f2, g2, h2 = (8, 9, 10, 11, 12, 13, 14, 15)
  let a1, b1, c1, d1, e1, f1, g1, h1 = (0, 1, 2, 3, 4, 5, 6, 7)
  let all_squares = List.init 64 Fun.id
  let in_relative_rank i c s = Rank.equal (rank s) @@ Rank.relative i c
  let rank_squares r = List.init 8 (fun i -> (r * 8) + i)

  let king_distance s1 s2 =
    Int.(max (abs @@ (file s1 - file s2)) (abs @@ (rank s1 - rank s2)))
end

module Piece = struct
  type piece_type = King | Queen | Rook | Bishop | Knight | Pawn
  type t = { piece_type : piece_type; piece_color : Color.t }

  let king = King
  let queen = Queen
  let rook = Rook
  let bishop = Bishop
  let knight = Knight
  let pawn = Pawn
  let color p = p.piece_color
  let piece_type p = p.piece_type

  let equal t1 t2 =
    piece_type t1 = piece_type t2 && Color.equal (color t1) (color t2)

  let make piece_color piece_type = { piece_type; piece_color }
  let wK = make Color.white king
  let wQ = make Color.white queen
  let wR = make Color.white rook
  let wB = make Color.white bishop
  let wN = make Color.white knight
  let wP = make Color.white pawn
  let bK = make Color.black king
  let bQ = make Color.black queen
  let bR = make Color.black rook
  let bB = make Color.black bishop
  let bN = make Color.black knight
  let bP = make Color.black pawn

  let piece_type_of_char c =
    match c with
    | 'k' -> King
    | 'q' -> Queen
    | 'r' -> Rook
    | 'b' -> Bishop
    | 'n' -> Knight
    | 'p' -> Pawn
    | _ -> raise @@ Invalid_argument ("Unknown piece_type: " ^ Char.escaped c)

  let piece_type_to_char = function
    | King -> 'k'
    | Queen -> 'q'
    | Rook -> 'r'
    | Bishop -> 'b'
    | Knight -> 'n'
    | Pawn -> 'p'

  let to_char t =
    if Color.is_black t.piece_color then piece_type_to_char t.piece_type
    else Char.uppercase_ascii @@ piece_type_to_char t.piece_type

  let of_char c =
    let l = Char.lowercase_ascii c in
    let piece_color = if l = c then Color.Black else Color.White in
    { piece_type = piece_type_of_char l; piece_color }
end

module Direction = struct
  let rank1 = Rank.relative 1 Color.white
  let rank8 = Rank.relative 8 Color.white
  let north s = if Square.rank s = rank8 then None else Some (s + 8)
  let south s = if Square.rank s = rank1 then None else Some (s - 8)
  let east s = if Square.file s = File.h then None else Some (s + 1)
  let west s = if Square.file s = File.a then None else Some (s - 1)
  let north_east s = Option.bind (north s) east
  let north_west s = Option.bind (north s) west
  let south_east s = Option.bind (south s) east
  let south_west s = Option.bind (south s) west

  let diag_neighbors s =
    List.filter_map
      (fun dir -> dir s)
      [ north_east; north_west; south_east; south_west ]

  let straight_neighbors s =
    List.filter_map (fun dir -> dir s) [ north; south; east; west ]

  let knight_neighbors s =
    [
      Option.bind (north_east s) north;
      Option.bind (north_east s) east;
      Option.bind (north_west s) north;
      Option.bind (north_west s) west;
      Option.bind (south_east s) south;
      Option.bind (south_east s) east;
      Option.bind (south_west s) south;
      Option.bind (south_west s) west;
    ]
    |> List.filter_map Fun.id
end

module Bitboard = struct
  type t = Int64.t

  let empty = 0L
  let add s bb = Int64.(logor bb @@ shift_left 1L s)
  let get s bb = Int64.(logand bb (shift_left 1L s) <> 0L)

  let to_string bb =
    List.init 8 (fun row ->
        List.init 8 (fun col ->
            let s = col + ((7 - row) * 8) in
            if get s bb then "x"
            else if Color.is_black (Square.color s) then "."
            else "_")
        |> String.concat " ")
    |> String.concat "\n"

  let print_bb bb = Format.printf "%s\n" @@ to_string bb
end

module SquareMap = Map.Make (Square)

(* A board is implemented as a map from squares to pieces *)
type t = Piece.t SquareMap.t

let equal = SquareMap.equal Piece.equal
let empty = SquareMap.empty
let squares = List.init 64 Fun.id
let files = List.init 8 Fun.id
let ranks = List.init 8 Fun.id

let of_pieces : (Piece.t * Square.t) list -> t =
  List.fold_left (fun t (p, s) -> SquareMap.add s p t) SquareMap.empty

let to_pieces t = SquareMap.bindings t |> List.map (fun (s, p) -> (p, s))

let white_pieces t =
  List.filter Piece.(fun (p, _) -> Color.is_white p.piece_color) (to_pieces t)

let black_pieces t =
  List.filter Piece.(fun (p, _) -> Color.is_black p.piece_color) (to_pieces t)

let color_pieces color =
  if Color.is_white color then white_pieces else black_pieces

let piece_at s t = SquareMap.find_opt s t
let set_piece (p, s) t = SquareMap.add s p t
let remove_piece s t = SquareMap.remove s t

let find_pieces p t =
  List.filter_map (fun (p', s) -> if p = p' then Some s else None) (to_pieces t)

let count ?square_color p t =
  SquareMap.filter
    (fun s p' ->
      match square_color with
      | None -> Piece.equal p p'
      | Some c -> Piece.equal p p' && Square.color s = c)
    t
  |> SquareMap.cardinal

let of_fen fen =
  String.fold_left
    (fun (pos, s) c ->
      if c = '/' then (pos, s - 16)
      else
        match int_of_string_opt (Char.escaped c) with
        | None -> (SquareMap.add s (Piece.of_char c) pos, s + 1)
        | Some i -> (pos, s + i))
    (SquareMap.empty, Square.a8)
    fen
  |> fst

let to_fen t =
  let flush_cnt cnt str = if cnt = 0 then str else str ^ string_of_int cnt in
  let rec aux ranks (rank, cnt) sq =
    if not (Square.is_valid sq) then ranks
    else
      let symb, cnt =
        match SquareMap.find_opt sq t with
        | None -> ("", cnt + 1)
        | Some p ->
            let p_str = Piece.to_char p |> Char.escaped in
            (flush_cnt cnt "" ^ p_str, 0)
      in
      let rank = rank ^ symb in
      if Square.file sq < 7 then aux ranks (rank, cnt) (sq + 1)
      else aux (flush_cnt cnt rank :: ranks) ("", 0) (sq + 1)
  in
  let ranks = aux [] ("", 0) 0 in
  let fen = String.concat "/" ranks in
  fen

let initial = of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
