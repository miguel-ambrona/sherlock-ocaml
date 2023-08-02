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

module Matrix = struct
  type t = int SquareMap.t

  let empty = SquareMap.empty
  let add = SquareMap.add
  let get s mat = SquareMap.find_opt s mat |> Option.value ~default:0

  let to_string matrices =
    let pp_char i =
      if i <= 26 then Char.chr (i + 64) else Char.chr (i + 64 + 6)
    in
    List.init 8 (fun row ->
        ">>  "
        ^ (List.map
             (fun mat ->
               List.init 8 (fun col ->
                   let s = col + ((7 - row) * 8) in
                   let i = get s mat in
                   if i <> 0 then pp_char i |> Char.escaped
                   else if Color.is_black (Square.color s) then "."
                   else "_")
               |> String.concat " ")
             matrices
          |> String.concat "   "))
    |> String.concat "\n"

  let print bb = Format.printf "%s\n>>\n" @@ to_string bb
end

let print_state (state : State.t) =
  let static_matrix =
    SquareSet.fold (fun s -> Matrix.add s 24) state.static Matrix.empty
  in
  let finals_mat, origins_mat, _ =
    SquareMap.fold
      (fun s ts (finals, origins, cnt) ->
        if SquareSet.cardinal ts = 1 then
          let t = SquareSet.choose ts in
          (Matrix.add s cnt finals, Matrix.add t cnt origins, cnt + 1)
        else (finals, origins, cnt))
      state.origins
      (Matrix.empty, Matrix.empty, 1)
  in
  Format.printf "\n\n\n";
  Format.printf ">> pos: %s\n" @@ Position.to_fen state.pos;
  Format.printf ">> proven illegal: %b\n" state.illegal;
  Format.printf ">> static:\n";
  Matrix.print [ static_matrix ];
  Format.printf ">> origins:\n";
  Matrix.print [ finals_mat; origins_mat ];
  Format.printf ">>       final             initial\n";
  Format.printf ">>\n";
  SquareMap.iter
    (fun s ts ->
      let p = Position.piece_at s state.pos |> Option.get in
      if SquareSet.cardinal ts <> 1 then
        Format.printf ">> %c%c%s comes from [%s]\n"
          (Color.to_char (Piece.color p))
          (Piece.to_char p |> Char.uppercase_ascii)
          (Square.to_string s)
          (String.concat ", "
             (SquareSet.elements ts |> List.map Square.to_string)))
    state.origins
