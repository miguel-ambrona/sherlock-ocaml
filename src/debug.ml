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

  let print matrices = Format.printf "%s\n>>\n" @@ to_string matrices
end

let print_state (state : State.t) =
  let char_of_int n = if n < 10 then 48 - 64 + n else 81 - 64 + n in
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
  let lower_captures_matrix, upper_captures_matrix =
    SquareMap.fold
      (fun o (lower, upper) (m_lower, m_upper) ->
        ( Matrix.add o (char_of_int lower) m_lower,
          Matrix.add o (char_of_int upper) m_upper ))
      state.captures
      (Matrix.empty, Matrix.empty)
  in
  let missing_matrix (missing : State.uncertain_square_set) =
    List.fold_left
      (fun m s ->
        if SquareSet.mem s missing.definite then Matrix.add s 24 m
        else if SquareSet.mem s missing.candidates then Matrix.add s (-1) m
        else m)
      Matrix.empty Board.squares
  in
  let white_missing = ColorMap.find Color.White state.missing in
  let black_missing = ColorMap.find Color.Black state.missing in
  let tombs_matrix c =
    let c_tombs = ColorMap.find c state.tombs in
    List.fold_left
      (fun matrix s ->
        let n = List.filter (Square.equal s) c_tombs |> List.length in
        Matrix.add s (char_of_int n) matrix)
      Matrix.empty c_tombs
  in
  Format.printf "\n\n\n";
  Format.printf ">> pos: %s\n" @@ Position.to_fen state.pos;
  Format.printf ">> proven illegal: %b\n" state.illegal;
  Format.printf ">> static:\n";
  Matrix.print [ static_matrix ];
  Format.printf ">> origins:\n";
  Matrix.print [ finals_mat; origins_mat ];
  Format.printf ">>       final             initial\n";
  Format.printf ">> destinies:\n";
  SquareMap.iter
    (fun o ts ->
      if not (SquareSet.mem o state.static) then
        Format.printf "%s -> {%s}\n" (Square.to_string o)
          (String.concat ", "
          @@ List.map Square.to_string (List.of_seq (SquareSet.to_seq ts))))
    state.destinies;
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
    state.origins;
  Format.printf "\n";
  Matrix.print [ lower_captures_matrix; upper_captures_matrix ];
  Format.printf ">>  min_nb_captures   max_nb_captures\n\n";
  Format.printf ">> missing:\n";
  Matrix.print [ missing_matrix white_missing; missing_matrix black_missing ];
  Format.printf ">>     white (%d)          black (%d)\n" white_missing.cardinal
    black_missing.cardinal;
  Format.printf ">>\n";
  Format.printf ">> tombs:\n";
  Matrix.print [ tombs_matrix Color.Black; tombs_matrix Color.White ];
  Format.printf ">>  white tombs (%d)   black tombs (%d)\n"
    white_missing.cardinal black_missing.cardinal;
  Format.printf ">>\n"
