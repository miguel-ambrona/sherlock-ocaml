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
module FenMap = Map.Make (String)

let table = ref FenMap.empty
let fetch_reason fen = FenMap.find_opt fen !table
let save_reason fen result = table := FenMap.add fen result !table

let all_rules =
  let open Legality.Rules in
  [
    (static_rule, "static_rule");
    (material_rule, "material_rule");
    (pawn_structure_rule, "pawn_structure_rule");
    (origins_rule, "origins_rule");
    (refine_origins_rule, "refine_origins_rule");
    (destinies_rule, "destinies");
    (knight_origins_rule, "knight_origins");
    (static_mobility_rule, "static_mobility");
    (static_king_rule, "static_king");
    (refine_static_king_rule, "refine_static_king");
    (pawn_on_3rd_rank_rule, "3rd rank");
    (route_from_origin_rule, "route from origin");
    (captures_rule, "captures");
    (too_many_captures_rule, "too many captures");
    (missing_rule, "missing");
    (tombs_rule, "tombs");
    (visiting_tombs_rules, "visiting tombs");
    (parity_rule, "parity");
  ]

let rec debug_apply ?(debug = false) state =
  let open Legality in
  let new_state =
    List.fold_left
      (fun (s : State.t) (rule, rule_name) ->
        if Option.is_some s.illegal then s
        else
          let s = rule s in
          if debug then (
            Format.printf "RULE: %s\n" rule_name;
            Debug.print_state s);
          s)
      state all_rules
  in
  if State.equal state new_state then state else debug_apply new_state

let rec is_illegal_with_reason ?(recursion_depth = 1) pos =
  let open Legality in
  let fen = Position.to_fen { pos with fullmove_number = 0 } in
  match fetch_reason fen with
  | Some res -> res
  | None ->
      (* Save anything for now, we will rewrite this result *)
      save_reason fen (Some "undefined");
      let res =
        if illegal_check pos then Some "illegal check"
        else
          match (debug_apply (State.init pos)).illegal with
          | Some reason -> Some reason
          | None ->
              if dangerous_retractions pos && recursion_depth > 0 then
                if
                  List.exists
                    (fun p ->
                      Option.is_none
                      @@ is_illegal_with_reason
                           ~recursion_depth:(recursion_depth - 1) p)
                    (Retraction.retracted pos)
                then None
                else Some "unretractable"
              else None
      in
      save_reason fen res;
      res

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
    | "legal" -> (
        let pos = Position.of_fen fen in
        match is_illegal_with_reason pos with
        | Some reason -> Format.printf "illegal (%s)\n" reason
        | None ->
            cnt := !cnt + 1;
            Format.printf "TBD\n")
    | _ -> raise @@ Invalid_argument "Unknown command");
    Format.printf "nsols %d\n" !cnt;
    Format.print_flush ();
    wait_for_command ())

and wait_for_command () = process_command @@ input_line stdin

let main = wait_for_command ()
