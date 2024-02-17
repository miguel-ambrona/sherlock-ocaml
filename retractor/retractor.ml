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

open Chess

let rec process_command cmd =
  let words = String.split_on_char ' ' cmd in
  let instruction = List.hd words in
  let fen = String.concat " " @@ List.tl words in
  let cnt = ref 0 in
  if instruction = "quit" then ()
  else (
    (match instruction with
    | "retract" ->
        let pos = Position.of_fen fen in
        let retractions = Retraction.pseudo_legal_retractions pos in
        List.iter
          (fun r ->
            List.iter
              (fun new_pos ->
                if not Position.(is_check { new_pos with turn = pos.turn }) then (
                  Format.printf "%s retraction %s\n" (Position.to_fen new_pos)
                    (Retraction.to_string r);
                  Format.print_flush ();
                  cnt := !cnt + 1)
                else ())
              (Retraction.apply pos r))
          retractions
    | "legal" ->
        let pos = Position.of_fen fen in
        if not (Legality.is_legal pos) then Format.printf "illegal\n"
        else (
          cnt := !cnt + 1;
          Format.printf "TBD\n")
    | _ -> raise @@ Invalid_argument "Unknown command");
    Format.printf "nsols %d\n" !cnt;
    Format.print_flush ();
    wait_for_command ())

and wait_for_command () = process_command @@ input_line stdin

let main = wait_for_command ()
