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
 fold_left (fun acc (start, option, final) -> if List.mem final acc then acc else final::acc) [] tlst

let rec getMove (delta: ('q, 's) transition list) (qs: 'q list) (s: 's option) = 
  (* for every state in qs looks thru delta and adds to list *)
  match qs with
  | [] -> []
  | h::t -> (List.filter(fun (s1, a, s2) -> s1 = h && s = a) delta) @ getMove delta t s
  
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  (* check delta list in for qs and s *)
  getState (getMove nfa.delta qs s)

let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  if qs = union(move nfa qs None) qs then qs else e_closure nfa (union (move nfa qs None) qs)
(* 
let final_check (state: 'q list) (finals: 'q list list) = 
  fold_left (fun acc s -> if s = state || acc = true then true else false) false finals

let rec isValid (dfa: ('q list, 's) nfa_t) prev char_lst = 
   fold_left fun *)
let rec isValid (dfa: ('q list, 's) nfa_t) (prev: 'q list) char_lst = 
  match char_lst with
  |[] -> prev
  |h::[] -> move dfa prev (Some h)
  |h::t -> isValid dfa (move dfa prev (Some h)) t

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let dfa = nfa_to_dfa nfa in
  let char_lst = explode s in

  fold_left (fun acc char -> move dfa acc char) (List.flatten dfa.q0) char_lst

  (* let end_state = isValid dfa.fs dfa.q0 (explode s) in

  if (final_check end_state dfa.fs) = true then true else false *)


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  fold_left (fun acc option -> e_closure nfa (move nfa qs (Some option)) :: acc) [] nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  fold_left (fun acc option -> if (e_closure nfa (move nfa qs (Some option))) = [] then acc else (qs, Some option, e_closure nfa (move nfa qs (Some option))) :: acc) [] nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let final_check =  fold_left (fun acc q -> if List.mem q nfa.fs || acc = true then true else false) false qs in

  if final_check = true then [qs] else []




let new_states_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  fold_left (fun acc option -> e_closure nfa (move nfa qs (Some option)) @ acc) [] nfa.sigma

let rec work (nfa: ('q,'s) nfa_t) (acc: 'q list list) (qs: 'q list list): 'q list list  = 
  if (union (new_states nfa (List.flatten qs)) qs) = qs then acc else work nfa (union (new_states nfa (List.flatten qs)) acc) (new_states nfa (List.flatten qs))
  


let test_work (nfa: ('q,'s) nfa_t): 'q list list = 

  let start = e_closure nfa [nfa.q0] in
  work nfa [start] [start]

(* let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
    
    match work with
    | [] -> dfa
    | h::t -> dfa.fs :: new_finals nfa *)

let getFinals (nfa: ('q,'s) nfa_t) (work: 'q list list) = 

  fold_left (fun acc s -> if new_finals nfa s = [] then acc else union (new_finals nfa s) acc) [] work

let get_delta (nfa: ('q,'s) nfa_t) (work: 'q list list) =
  fold_left (fun acc s -> union (new_trans nfa s) acc) [] work
    
let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  
   {
    sigma = nfa.sigma;
    qs = test_work nfa;
    q0 = e_closure nfa [nfa.q0];
    fs = getFinals nfa (test_work nfa);
    delta = get_delta nfa (test_work nfa);
  }


let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let dfa = nfa_to_dfa nfa in

  let end_state = isValid dfa [dfa.q0] (explode s) in

  if (final_check end_state [dfa.fs]) = true then true else false