(* linktype as real list *)
(* replaced Link.fold_left with List.iter *)

let lcs al bl = (* val lcs: 'a list -> 'a list -> (int * int) list *)

  let rec trim = function
    (n, a::al, b::bl) when a = b -> trim (n+1, al, bl)
    | arg -> arg in

  let mc, al, bl = trim (0, al, bl) in
  let nc, ar, bl = trim (0, List.rev al, List.rev bl) in (* efficient indeed? *)
  let al = if nc > 0 then List.rev ar else al in
  (* bl gets reversed. *)

  let ac = List.length al in
  let bc = List.length bl in
  let cc = min ac bc in
  let thresh = Array.make cc (-1) in
  let links = Array.make cc [] in
  let lcr = ref 0 in (* last index of links *)
  (* The maximum sizes for thresh and links must be the greatest length of the
     possible LCS, which is cc. However for the sake of efficiency reasons, the
     following codes were written under the assumption that thresh.(0) = -1 and
     links.(0) = []. So the actual sizes for thresh and links should have been
     cc+1 for those extra first elements. But actually we know that members of
     al and bl are different in at least one position. Otherwise the whole al
     and bl would be the same and we did not need thresh or links at all.
     That's why cc instead of cc+1 is used here for the sizes of thresh and
     links.
  *)

  let i = ref 0 in (* index thru al *)
  let j = ref 0 in (* index thru bl *)

  let replaceNextGreaterWith h =
    let rec loop l h =
      if l > h then begin
	thresh.(l) <- !j;
	links.(l) <- (!i, !j) :: links.(l-1);
	if l > !lcr then lcr := l;
	l (* return *)
      end

      else
	//let m = (l+h+h+h) asr 2 in (* m = (l+3*h)/4 *)
	(* not so efficient or even less efficient than below *)
	let m = (l+h) asr 1 in (* m = (l+3*h)/4 *)
	(* m can be set to any value in the range from l to h *)
	let found = thresh.(m) in
	if !j > found then
	  loop (m+1) h
	else if !j < found then
	  loop l (m-1)
	else
	  m (* return *)
    in
      if !j < thresh.(h) then
	if !j > thresh.(h-1) then
	  loop h (h-1) (* optimization: mostly this is the case *)
	else
	  loop 0 (h-1)
      else
	loop h h in

  (* Compute the LCS *)
  (* Substituting List.fold_left for List.iter by making i, j or k be
     arguments for functions rather than local variables is far less
     efficient than this imperative way.
  *)
  let iter_a a =
    let k = ref !lcr in (* !lcr is the initial value of k *)
    let iter_b b =
      j := !j-1;
      if a = b then
	k := replaceNextGreaterWith !k
    in
      j := bc;
      ignore(List.iter iter_b bl);
      i := !i+1
  in
    List.iter iter_a al;
  
  (* Construct the hunks *)
  (* hunks are:
     (0,n) = n items added (right-only items)
     (m,0) = m items removed (left-only items)
     (m,n) = m items changed into n items
     (-1,n) = n same items
  *)
  let tail =
    if Array.length links > 0 then begin
      let rec hunks ans i j = function
	[] -> (i, j) :: ans
	  (* always either i > 0 or j > 0;
	     otherwise, we would have Array.length links = 0 *)
	| (i1, j1) :: link ->
	  matched ((i-1-i1, j-1-j1) :: ans) 1 i1 j1 link
	  (* always either i1 < i-1 or j1 < j-1;
	     the case for i1 = i-1 && j1 = j-1 is processed within matched()
	     and i1 = ac-1 && j1 = bc-1 is already considered as nc = 1 *)

      and matched ans n i j = function (* always n > 0 *)
	(i1, j1) :: link when i1 = i-1 && j1 = j-1 ->
	  matched ans (n+1) i1 j1 link
	| link -> hunks ((-1, n) :: ans) i j link (* (i1, j1) unprocessed *)

      in
	(* always !lcr >= 0 *)
	hunks (if nc > 0 then [(-1, nc)] else []) ac bc links.(!lcr)
    end
    else []

  in
    if mc > 0 then (-1, mc) :: tail else tail

  (* length of lcs = mc + !lcr + nc *)



let explode s = (* val explode: string -> char list *)
  let rec aux ans n =
    if n > 0 then aux (s.[n-1] :: ans) (n-1) else ans
  in aux [] (String.length s)

let implode l = (* val implode: char list -> string *)
  let s = Buffer.create (List.length l) in
  let rec aux = function
    [] -> Buffer.contents s
    | c::cs -> Buffer.add_char s c; aux cs
  in aux l

let test a b h =
  h = lcs (explode a) (explode b)

let _ = (* some tests *)
if
  test "abcehjlmnp" "bcdefjklmrst"
    [1,0; -1,2; 0,1; -1,1; 1,1; -1,1; 0,1; -1,2; 2,3] &&

  test "abcehjlmnp" "abcdefjklmrst"
    [-1,3; 0,1; -1,1; 1,1; -1,1; 0,1; -1,2; 2,3] &&

  test "abcehjlmnp" "bcdefjklmrstp"
    [1,0; -1,2; 0,1; -1,1; 1,1; -1,1; 0,1; -1,2; 1,3; -1,1] &&

  test "abcehjlmnp" "abcdefjklmrstp"
    [-1,3; 0,1; -1,1; 1,1; -1,1; 0,1; -1,2; 1,3; -1,1] &&

  test "abcb" "bdcaba"
    [1,0; -1,1; 0,1; -1,1; 0,1; -1,1; 0,1] &&

  test "cehjlmnpr" "abcdfgjklmst"
    [0,2; -1,1; 2,3; -1,1; 0,1; -1,2; 3,2] &&

  test "abcehjlmnp" "abcehxlmnp"
    [-1,5; 1,1; -1,4] &&

  test "abcehjlmnp" "xbcehjlmnp"
    [1,1; -1,9] &&

  test "abcehjlmnp" "abcehjlmnx"
    [-1,9; 1,1] &&

  test "abcehjlmnp" "abcehjlmnp"
    [-1,10] &&

  test "abcehjlmnp" "xxxxx"
    [10,5] &&

  test "xxxxx" "abcehjlmnp"
    [5,10] &&

  test "abcehjlmnp" ""
    [] &&

  test "" "abcehjlmnp"
    []

then
  print_endline "Ok"

(*
open Printf

let _ =
  let f1 = open_in "mccdma1.c" in
  let f2 = open_in "mccdma2.c" in
  let f1t = ref [] in
  let f2t = ref [] in

  try
    while true do f1t := (input_line f1) :: !f1t done
  with End_of_file -> ();
  f1t := List.rev !f1t;
  try
    while true do f2t := (input_line f2) :: !f2t done
  with End_of_file -> ();
  f2t := List.rev !f2t;

  for i=1 to 3 do ignore(lcs !f1t !f2t) done
*)
(*
  let hunks = lcs !f1t !f2t in
  let rec print_hunks = function
    (m, n) :: tail -> printf "%d %d\n" m n; print_hunks tail
    | _ -> ()
  in
    print_hunks hunks
*)
