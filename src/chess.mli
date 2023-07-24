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
