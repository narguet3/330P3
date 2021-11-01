open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec getState tlst = 
  match tlst with 
  |[] -> []
  |(s1, a, s2)::t -> s2 :: getState(t)

let rec getMove (delta: ('q, 's) transition list) (qs: 'q list) (s: 's option) = 
  (* for every state in qs looks thru delta and adds to list *)
  match qs with
  | [] -> []
  | h::t -> (List.filter(fun (s1, a, s2) -> s1 = h && s = a) delta) @ getMove delta t s
  
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  (* check delta list in for qs and s *)
  match nfa with
  | {sigma = si; delta = d; _} -> getState (getMove d qs s)


let hasState n lst = 
  List.exists n lst

let rec fold f a xs = match xs with
| [] -> a
| x :: xt -> fold f (f a x) xt

let contains_elem lst e = 
    fold (fun a lst -> if lst = e || a = true then true else false) false lst


let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let nlst = move nfa qs None in

  fold (fun acc a -> if contains_elem acc a then acc else a::acc) nlst qs


let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  failwith "unimplemented"

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  failwith "unimplemented"

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  failwith "unimplemented"
