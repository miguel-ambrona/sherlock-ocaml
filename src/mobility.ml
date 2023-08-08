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

module Square = Board.Square

module Vertex = struct
  type t = Square.t

  let compare = Square.compare
  let equal = Square.equal
  let hash s = s
end

module E = struct
  include Int

  let default = 0
end

module Weight = struct
  include Int

  type edge = Vertex.t * int * Vertex.t

  let weight (_, w, _) = w
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (Vertex) (E)
module Path = Graph.Path.Dijkstra (G) (Weight)

let all_square_pairs =
  List.concat_map
    (fun s -> List.map (fun t -> (s, t)) Board.squares)
    Board.squares

let build_graph ~weight square_pairs =
  List.fold_left
    (fun g (s, t) -> G.add_edge_e g (s, weight s t, t))
    G.empty square_pairs

let fast_piece_graph ?(weight = fun _ _ -> 0) cond =
  build_graph ~weight
  @@ List.filter (fun (s, t) -> s <> t && cond s t) all_square_pairs

let slow_piece_graph ?(weight = fun _ _ -> 0) targets =
  build_graph ~weight
  @@ List.concat_map
       (fun s -> List.map (fun t -> (s, t)) @@ targets s)
       Board.squares

let piece_graph p =
  let open Board.Direction in
  let c = Board.Piece.color p in
  match Board.Piece.piece_type p with
  | King -> slow_piece_graph king_neighbors
  | Queen -> fast_piece_graph Square.in_same_line
  | Rook -> fast_piece_graph Square.in_same_file_or_rank
  | Bishop -> fast_piece_graph Square.in_same_diagonal
  | Knight -> slow_piece_graph knight_neighbors
  | Pawn ->
      let weight s t = if Square.(file s = file t) then 0 else 1 in
      slow_piece_graph ~weight (pawn_forward_targets c)

let distance ~infty g s t =
  try snd @@ Path.shortest_path g s t with Not_found -> infty

let filter_edges f g =
  G.fold_edges (fun s t g -> if f s t then g else G.remove_edge g s t) g g
