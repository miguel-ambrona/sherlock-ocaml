(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2022 Miguel Ambrona <mac.ambrona@gmail.com>                 *)
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

(** Colors (white and black) are an attribute of board pieces and squares. *)
module Color : sig
  type t
  (** The type of colors. *)

  val white : t
  (** The white color. *)

  val black : t
  (** The black color. *)

  val equal : t -> t -> bool
  (** [equal c1 c2] returns [true] iff colors [c1] and [c2] are the same. *)

  val negate : t -> t
  (** [negate white] returns [black] whereas [negate black] returns [white]. *)

  val is_white : t -> bool
  (** Alias for [equal white]. *)

  val is_black : t -> bool
  (** Alias for [equal black]. *)

  val of_char : char -> t
  (** [of_char c] converts char [c] into a color.
      @raise Invalid_argument if [c] is not ['w'] nor ['b']. *)

  val to_char : t -> char
  (** Return ['w'] given [white] or ['b'] given [black]. *)
end

(** Board structure and operations. *)
module Board : sig
  (** Module to represent files in a chess board. *)
  module File : sig
    type t
    (** The type of files. *)

    val equal : t -> t -> bool
    (** [equal f1 f2] returns [true] iff files [f1] and [f2] are the same. *)

    val a : t
    (** The A file. *)

    val b : t
    (** The B file. *)

    val c : t
    (** The C file. *)

    val d : t
    (** The D file. *)

    val e : t
    (** The E file. *)

    val f : t
    (** The F file. *)

    val g : t
    (** The G file. *)

    val h : t
    (** The H file. *)

    val of_char : char -> t
    (** [of_char c] converts character [c] into a file.
        @raise Invalid_argument if [c] is not a character in the range a--h. *)

    val to_char : t -> char
    (** Convert the given file into a letter character in the range a--h. *)
  end

  (** Module to represent ranks in a chess board. *)
  module Rank : sig
    type t
    (** The type of ranks. *)

    val equal : t -> t -> bool
    (** [equal r1 r2] returns [true] iff ranks [r1] and [r2] are the same. *)

    val relative : int -> Color.t -> t
    (** [relative i c] returns the i-th relative rank from the perpective
        of the player holding the pieces of color [c].
        @raise Invalid_argument if [i] is not an integer in the range 1--8. *)

    val of_char : char -> t
    (** [of_char c] converts character [c] into a rank.
        @raise Invalid_argument if [c] is not a character in the range 1--8. *)

    val to_char : t -> char
    (** Convert the given file into an integer character in the range 1--8. *)
  end

  (** Module to represent squares in a chess board. *)
  module Square : sig
    type t
    (** The type of squares. *)

    val color : t -> Color.t
    (** The color of the given square. *)

    val file : t -> File.t
    (** The file of the given square. *)

    val rank : t -> Rank.t
    (** The rank of the given square. *)

    val equal : t -> t -> bool
    (** [equal s1 s2] returns [true] iff squares [s1] and [s2] are the same. *)

    val of_string : string -> t
    (** [of_string str] converts string [str] into a square parsing it in
      algebraic notation.
      @raise Invalid_argument if [str] is not formed by a letter in the range
      a--h followed by an integer in the range 1--8. *)

    val to_string : t -> string
    (** Convert the given square into a string in algebraic notation. *)

    val in_relative_rank : int -> Color.t -> t -> bool
    (** [in_relative_rank i c s] returns [true] iff square [s] is in the i-th
        relative rank from the perpective of the player holding the pieces
        of color [c].
        @raise Invalid_argument if [i] is not an integer in the range 1--8. *)

    val rank_squares : Rank.t -> t list
    (** [rank_squares r] returns the list of squares in the given rank. *)

    val king_distance : t -> t -> int
    (** [king_distance s1 s2] returns the number of moves that it takes for a
        king to move from square [s1] to square [s2] over an empty board. *)
  end

  (** Module to represent pieces in a chess board. *)
  module Piece : sig
    type t
    (** The type of pieces. *)

    (** {1 Piece Types} *)

    type piece_type

    val king : piece_type
    (** The piece type of kings. *)

    val queen : piece_type
    (** The piece type of queens. *)

    val rook : piece_type
    (** The piece type of rooks. *)

    val bishop : piece_type
    (** The piece type of bishops. *)

    val knight : piece_type
    (** The piece type of knights. *)

    val pawn : piece_type
    (** The piece type of pawns. *)

    (** {1 Pieces} *)

    val color : t -> Color.t
    (** The color of the given piece. *)

    val piece_type : t -> piece_type
    (** The type of the given piece. *)

    val equal : t -> t -> bool
    (** [equal p1 p2] returns [true] iff pieces [p1] and [p2] are the same. *)

    val make : Color.t -> piece_type -> t
    (** Create a piece of the given color and the given piece type. *)

    val piece_type_of_char : char -> piece_type
    (** [piece_type_of_char c] converts character [c] into a piece type.
        @raise Invalid_argument if [c], is different from
        ['k', 'q', 'r', 'b', 'n', 'p']. *)

    val piece_type_to_char : piece_type -> char
    (** Convert the given piece type into a character following the standards of
        the FEN (Forsyth-Edwards Notation). *)

    val of_char : char -> t
    (** [of_char c] converts character [c] into a piece.
        @raise Invalid_argument if [c], in lowercase, is different from
        ['k', 'q', 'r', 'b', 'n', 'p']. *)

    val to_char : t -> char
    (** Convert the given piece into a character following the standards of the
        FEN (Forsyth-Edwards Notation): uppercase for white pieces and lowercase
        for black ones. *)
  end

  module Direction : sig
    val north : Square.t -> Square.t option
    (** [north s] returns the square in the same file and next rank
        relative to [s], or [None] if such square does not exist. *)

    val south : Square.t -> Square.t option
    (** [south s] returns the square in the same file and previous rank
        relative to [s], or [None] if such square does not exist. *)

    val east : Square.t -> Square.t option
    (** [east s] returns the square in the previous file and same rank
        relative to [s], or [None] if such square does not exist. *)

    val west : Square.t -> Square.t option
    (** [west s] returns the square in the next file and same rank
        relative to [s], or [None] if such square does not exist. *)

    val north_east : Square.t -> Square.t option
    (** [north_east s] returns the square in the previous file and next rank
        relative to [s], or [None] if such square does not exist. *)

    val north_west : Square.t -> Square.t option
    (** [north_west s] returns the square in the next file and next rank
        relative to [s], or [None] if such square does not exist. *)

    val south_east : Square.t -> Square.t option
    (** [south_east s] returns the square in the previous file and previous rank
        relative to [s], or [None] if such square does not exist. *)

    val south_west : Square.t -> Square.t option
    (** [south_west s] returns the square in the next file and previous rank
        relative to [s], or [None] if such square does not exist. *)
  end

  type t
  (** The type of boards. *)

  val equal : t -> t -> bool
  (** [equal b1 b2] returns [true] iff boards [b1] and [b2] are the same. *)

  val empty : t
  (** The empty board. *)

  val initial : t
  (** The initial board of a chess game. *)

  val squares : Square.t list
  (** A list with all the squares of the board in order A1-H1, ..., A8-H8. *)

  val files : File.t list
  (** A list with all the files in alphabetical order. *)

  val ranks : Rank.t list
  (** The list with all the ranks in order from White's perspective. *)

  val of_fen : string -> t
  (** Convert a string into a board by parsing it as the first component of a
      FEN (Forsyth-Edwards Notation).
      @raise Invalid_argument if the given FEN is invalid. *)

  val to_fen : t -> string
  (** Convert a board into (the first component of) a FEN string. *)

  val piece_at : Square.t -> t -> Piece.t option
  (** [piece_at s board] returns the piece on square [s] in [board] or [None] if
      square [s] is empty. *)

  val set_piece : Piece.t * Square.t -> t -> t
  (** [set_piece (p,s) board] sets piece [p] on square [s] in [board].
      It destroys any previously existing piece on [s]. *)

  val remove_piece : Square.t -> t -> t
  (** [remove_piece s board] removes any existing piece on [s] in [board].
      If [s] was not occupied, [board] is returned unchanged. *)

  val of_pieces : (Piece.t * Square.t) list -> t
  (** Create a board from a list of pieces and their location. *)

  val to_pieces : t -> (Piece.t * Square.t) list
  (** Return the list of pieces and their locations on the given board. *)

  val count : ?square_color:Color.t -> Piece.t -> t -> int
  (** [count ~square_color p board] returns the number of pieces equal to [p]
      in [board], considering only those over squares of color [square_color]
      if such optional argument is specified. *)
end

(** Position structure and operations. A position consists of a board together
    with auxiliary information to store the turn, castling/en-passant rights,
    the halfmove clock and the fullmove number. *)
module Position : sig
  type t
  (** The type of positions. *)

  val initial : t
  (** The initial position of a chess game. *)

  val equal : t -> t -> bool
  (** [equal pos1 pos2] returns [true] iff positions [pos1] and [pos2] are
      the same. *)

  val of_fen : string -> t
  (** Convert a string into a position by parsing it in
      Forsyth-Edwards Notation.
      @raise Invalid_argument if the given FEN is invalid. *)

  val to_fen : t -> string
  (** Convert a position into a FEN string. *)

  val board : t -> Board.t
  (** Return the board of the given position. *)

  val pieces : t -> (Board.Piece.t * Board.Square.t) list
  (** Return the list of pieces and their locations on the given position. *)
end

module Legality : sig
  val is_legal : Position.t -> bool
end
