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

module Test_Color = struct
  open Color

  let test_equal () =
    assert (equal white white);
    assert (equal black black);
    assert (not (equal white black));
    assert (not (equal black white))

  let test_negate () =
    assert (equal (negate white) black);
    assert (equal (negate black) white)

  let test_is_white () =
    assert (is_white white);
    assert (not @@ is_white black)

  let test_is_black () =
    assert (is_black black);
    assert (not @@ is_black white)

  let test_of_char () =
    assert (is_white @@ of_char 'w');
    assert (is_black @@ of_char 'b');
    try ignore @@ of_char 'n' with
    | Invalid_argument _ -> ()
    | _ -> assert false

  let test_to_char () =
    assert ('w' = to_char white);
    assert ('b' = to_char black)

  let tests =
    Alcotest.
      ( "Color",
        [
          test_case "equal" `Quick test_equal;
          test_case "negate" `Quick test_negate;
          test_case "is_white" `Quick test_is_white;
          test_case "is_black" `Quick test_is_black;
          test_case "of_char" `Quick test_of_char;
          test_case "to_char" `Quick test_to_char;
        ] )
end

let () =
  let open Alcotest in
  run "Color" [ Test_Color.tests ]
