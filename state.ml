#use "whileB.ml";;

(* On utilisera l'état défini ci-après dans tout le programme *)
let state = true::false::false::true::true::[]

(* Permet d'initialiser toutes les variables d'un état à 0 *)
let rec initState (s:bool list) : bool list =
  match s with
  | [] -> []
  | x::q -> false::(initState q)

let _ = assert(initState state = [false;false;false;false;false])

(* Permet de lire la valeur de la variable x dans l'état s *)
let rec get (x:int) (s:bool list) : bool =
  match x,s with
  | 0, v::s -> v
  | i , x::l1 -> get (i-1) l1
  | _, [] -> false

let _ = assert((get (0) state) = true)
let _ = assert((get (1) state) = false)
let _ = assert((get (2) state) = false)
let _ = assert((get (3) state) = true)
let _ = assert((get (4) state) = true)

(* Par défaut on renvoie false *)
let _ = assert((get (5) state) = false)

(* Permet de mettre à jour la variable v à la valeur n dans l'état s *) 
let rec update (s:bool list) (v:int) (n:bool): bool list =
  match v,s with
  | 0   , a :: l1 -> n :: l1
  | 0   , nil     -> n :: nil
  | i, a :: l1 -> a :: (update l1 (i-1) n)
  | i, nil     -> false :: (update nil (i-1) n)

let _ = assert((get (0) (update state 0 false)) = false)
let _ = assert((get (5) (update state 5 true)) = true)

(* Permet d'évaluer une expression de type bexp *)
let rec evalB = fun b s ->
  match b with
  | Bco(x) -> x
  | Bva(v) -> (get (Char.code v - Char.code 'a') s)
  | Bneg(n) -> not (evalB n s)
  | Band(x1, x2) -> (evalB x1 s) && (evalB x2 s)
  | Bor(x1, x2) -> (evalB x1 s) || (evalB x2 s)

let _ = evalB (Bva('b')) state
let _ = evalB (Bneg(Bva('a'))) state
let _ = evalB (Band(Bva('a'), Bva('b'))) state
let _ = evalB (Bor(Bva('a'), Bva('b'))) state

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

(* On créer ici deux programmes *)
let progAffect = pr_Programme (list_of_string "a:=0;b:=1;c:=0")
let progIf = pr_Programme (list_of_string "a:=0; b:=1; c:=a+b; i(c){a:=c}{a:=!a}")

(* Répond à la question 2.2.1 et 2.2.2 *)
let _ = assert((let (a,s) = progAffect in evalW a []) = [false;true;false])
let _ = assert((let (a,s) = progIf in evalW a []) = [true;true;true])

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

(* Nous testons ici l'exécution pas-à-pas du programme progAffect i.e la première affectation a:=0 *)
let _ = assert((let (a,s) = progAffect in faire_un_pas a []) = Inter (Seq (Affect ('b', Bco true), Affect ('c', Bco false)), [false]))

(* Permet d'exécuter entièrement un programme p dans l'état s *)
let rec executer = fun p s ->
  match faire_un_pas p s with
  | Final s -> s
  | Inter (p',s') -> executer p' s' 
  
let _ = assert((let (a,s) = progAffect in executer a []) = [false;true;false])
let _ = assert((let (a,s) = progIf in executer a []) = [true;true;true])
