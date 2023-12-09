open Bexp
open WhileB
open Anacomb

(* Permet d'initialiser toutes les variables d'un état à 0 *)
let rec initState (s:bool list) : bool list =
  match s with
  | [] -> []
  | x::q -> false::(initState q)

(* Permet de lire la valeur de la variable x dans l'état s *)
let rec get (x:int) (s:bool list) : bool =
  match x,s with
  | 0, v::s -> v
  | i , x::l1 -> get (i-1) l1
  | _, [] -> false


(* Permet de mettre à jour la variable v à la valeur n dans l'état s *) 
let rec update (s:bool list) (v:int) (n:bool): bool list =
  match v,s with
  | 0   , a :: l1 -> n :: l1
  | 0   , nil     -> n :: nil
  | i, a :: l1 -> a :: (update l1 (i-1) n)
  | i, nil     -> false :: (update nil (i-1) n)

(* Permet d'évaluer une expression de type bexp *)
let rec evalB = fun b s ->
  match b with
  | Bco(x) -> x
  | Bva(v) -> (get (Char.code v - Char.code 'a') s)
  | Bneg(n) -> not (evalB n s)
  | Band(x1, x2) -> (evalB x1 s) && (evalB x2 s)
  | Bor(x1, x2) -> (evalB x1 s) || (evalB x2 s)

(* Permet d'évaluer une expression de type whileb *)
let rec evalW = fun w s ->
  match w with
  | Skip -> s
  | Seq (i1, i2) -> let s' = evalW i1 s in evalW i2 s'
  | Affect (c, b2) -> update s (Char.code c - Char.code 'a')  (evalB b2 s)
  | If (c, i1, i2) ->
     if (evalB c s) then
       evalW i1 s
     else
       evalW i2 s
  | While(c,i1) ->
     if (evalB c s) then
       let s' = evalW i1 s in evalW (While (c,i1)) s'
     else
       s;;

type config =
  | Inter of whileB * (bool list)
  | Final of (bool list)

(* Permet d'effectuer un pas dans l'exécution du programme p dans l'état s *)
let faire_un_pas = fun p s ->
  match p with
  | Skip -> Final s
  | Affect (c, b2) -> let s' = evalW (Affect (c,b2)) s in Final s'
  | Seq (i1, i2) -> let s' = (evalW i1 s) in Inter (i2, s')
  | If (c, i1, i2) ->
     if evalB c s then
       let s' = (evalW i1 s) in Inter (i1, s')
     else
       let s' = (evalW i2 s) in Inter (i2, s')
  | While (c, i1) ->
     if evalB c s then
       let s' = evalW i1 s in Inter ((While (c, i1)), s')
     else
       Final s

(* Permet d'exécuter entièrement un programme p dans l'état s *)
let rec executer = fun p s ->
  match faire_un_pas p s with
  | Final s -> s
  | Inter (p',s') -> executer p' s' 
  
(* Permet d'effectuer un pas dans l'exécution du programme p dans l'état s *)
let faire_un_pas_avec_cpt = fun p cpt s ->
  match p with
  | Skip -> (Final s, cpt)
  | Affect (c, b2) -> let s' = evalW (Affect (c,b2)) s in (Final s', cpt+1)
  | Seq (i1, i2) -> let s' = (evalW i1 s) in (Inter (i2, s'), cpt+1)
  | If (c, i1, i2) ->
     if evalB c s then
       let s' = (evalW i1 s) in (Inter (i1, s'), cpt)
     else
       let s' = (evalW i2 s) in (Inter (i2, s'), cpt)
  | While (c, i1) ->
     if evalB c s then
       let s' = evalW i1 s in (Inter ((While (c, i1)), s'), cpt)
     else
      (Final s, cpt+1)

let rec executer_avec_cpt = fun p cpt s ->
  match faire_un_pas_avec_cpt p cpt s with
  | Final s, c -> (s,c)
  | Inter (p',s'), c -> executer_avec_cpt p' c s'

let print_interactive_result result = let () = Printf.printf "[" in
  let rec loop = function
    | [] -> Printf.printf "]\n";
    | true :: rest -> if rest=[] then (Printf.printf "1"; loop rest) else (Printf.printf "1; "; loop rest)
    | false :: rest -> if rest=[] then (Printf.printf "0"; loop rest) else (Printf.printf "0; "; loop rest)
  in
  loop result

let rec execution_interactive = fun p s cpt ->
  match faire_un_pas_avec_cpt p s cpt with
  | Final s', c -> Printf.printf "Final state: "; print_interactive_result s';
    Printf.printf "\nThe program has executed %d instruction(s).\n" c;
    s'
  | Inter (p', s'), c ->
    Printf.printf ("Step %d: ") c;
    print_interactive_result s';
    ignore (read_line ());
    execution_interactive p' c s'
