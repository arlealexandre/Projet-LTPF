open Anacomb
open VariableAnalyseur
open AexpAnalyseur
open BexpAnalyseur
open WhileAnalyseur

type variable = typeV * (char list)

type valeur =
  | Null
  | BoolExp of bool
  | ArithExp of int
  | Function of valeur
  | VarName of variable

type state = ((variable * valeur) list)

exception VarNotFound of string
exception ExceptionType of string
exception VarAlreadyExists of string
exception BadWriting of string

(* On utilisera l'état défini ci-après dans tout le programme *)

let etat : state = ((Int,'a'::'1'::[]),ArithExp(5))::((Int,'a'::'2'::[]),ArithExp(-1))::((Bool,'b'::'1'::[]),BoolExp(true))::[]

(* Permet d'initialiser toutes les variables d'un état à 0 
let rec initState (s:bool list) : bool list =
  match s with
  | [] -> []
  | x::q -> false::(initState q)

let _ = assert(initState state = [false;false;false;false;false])*)

let rec equals (x:char list) (x':char list) : bool =
  match x, x' with
  | [],[] -> true
  | (c::q),(c'::q') when (c=c') -> equals q q'
  | _,_ -> false

let contains (x:char list) (v:variable) : bool =
  match v with
  | (t,l) -> equals x l

(* Permet de lire la valeur de la variable x dans l'état s *)
let rec get (x:char list) (s:state) : valeur =
  match s with
  | [] -> raise (VarNotFound "var not found")
  | (var,vale)::q when (contains x var) -> vale
  | (var,vale)::q -> get x q

let _ = get (list_of_string "a2")  etat

let rec verifType (t:typeV) (v:valeur) : bool =
  match t,v with
  | (Int, ArithExp (x)) -> true
  | (Bool, BoolExp (x)) -> true
  | (Fun, _) -> true
  | (_,VarName (t',x)) -> t = t'
  | (_,_) -> false

let create (s:state) ((t,v):variable) (n:valeur) : state =
  let rec f = fun (s':state) ((t',v'):variable) (n':valeur) : state ->
                                                          match s' with
                                                          | [] -> if (verifType t n) then
                                                                    [((t,v),n)]
                                                                  else
                                                                    raise (ExceptionType "mauvais type")
                                                          | x::q -> x::(f q (t',v') n') in
                                                          
  try let _ = (get v s) in raise (VarAlreadyExists "var existe déjà") with
  | VarNotFound (i) -> f s (t,v) n
  | VarAlreadyExists (i) -> raise (VarAlreadyExists "var existe déjà")

let _ = create etat (Int, list_of_string "a18") (ArithExp (2))

(* Permet de mettre à jour la variable v à la valeur n dans l'état s *) 
let rec update (s:state) (v:char list) (n:valeur): state =
  match s with
  | [] -> raise (VarNotFound "var not found")
  | (var,vale)::q when (contains v var) ->
     let (t,v') = var in
     if (verifType t n) then
       (var,n)::q
     else
       raise (ExceptionType "mauvais type")
  | (var,vale)::q -> update q v n

let _ = update etat (list_of_string "a1") (ArithExp (-5))

let rec evalA = fun (a:aexp) (s:state) ->
  match a with
  | Acst(i) -> i
  | Ava(v) -> let t = get v s in
              (match t with
               | ArithExp x -> x
               | _ -> raise (ExceptionType "mauvais type"))
  | Apl(a1,a2) -> (evalA a1 s) + (evalA a2 s) 
  | Amo(a1,a2) -> (evalA a1 s) - (evalA a2 s)
  | Amu(a1,a2) -> (evalA a1 s) * (evalA a2 s)
  | Adi(a1,a2) -> (evalA a1 s) / (evalA a2 s)


(* Permet d'évaluer une expression de type bexp *)
let rec evalB = fun (b:bexp) (s:state) ->
  match b with
  | Bco(x) -> x
  | Bva(v) -> let v' = get v s in
              (match v' with
              | BoolExp x -> x
              | _ -> raise (ExceptionType "mauvais type"))
  | Bneg(n) -> not (evalB n s)
  | Band(x1, x2) -> (evalB x1 s) && (evalB x2 s)
  | Bor(x1, x2) -> (evalB x1 s) || (evalB x2 s)
  | Beq(a1,a2) -> (evalA a1 s) = (evalA a2 s)
  | Bsup(a1,a2) -> (evalA a1 s) > (evalA a2 s)
  | Bsupeg(a1,a2) -> (evalA a1 s) >= (evalA a2 s)
  | Binf(a1,a2) -> (evalA a1 s) < (evalA a2 s)
  | Binfeg(a1,a2) -> (evalA a1 s) <= (evalA a2 s)

let _ = evalA (Acst(5)) etat
let _ = evalA (Apl(Acst(5),Ava((list_of_string "a1")))) etat
let _ = evalB (Bor(Bco(false),Bva((list_of_string "b1")))) etat

let rec evalR = fun (r:return) (s:state) ->
  match r with
  | Null -> Null
  | ReturnI (a) -> ArithExp (evalA a s)
  | ReturnB (b) -> BoolExp (evalB b s)
  | ReturnV (v) -> get v s

(* Permet d'évaluer une expression de type whileb *)
let rec evalW = fun w s ->
  match w with
  | Skip -> s
  | Seq (i1, i2) -> let s' = evalW i1 s in evalW i2 s'
  | Declaration (t, p) -> (match p with
                           | AffectI (c, a) -> create s (t,c) (ArithExp (evalA a s))
                           | AffectB (c, b) -> create s (t,c) (BoolExp (evalB b s))
                           | AffectF (c, f, r) -> let s' = (evalW f s) in create s' (t,c) (evalR r s')
                           | _ -> raise (BadWriting "code mal écrit"))
                            
  | AffectI (c, a) -> update s c (ArithExp (evalA a s))
  | AffectB (c, b) -> update s c (BoolExp (evalB b s))
  | AffectF (c, f, r) -> let s' = (evalW f s) in update s' c (Function (evalR r s'))
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
let progAffect = pr_programme (list_of_string "int a := 5+2; bool b := true; fun f := { return a };")
let progFun = pr_programme (list_of_string "int a := 1; fun f := { return a }; a := a + f")

let progTest = pr_programme (list_of_string "fun f := { return 1}")

let _ = let (a,s) = progTest in evalW a []

let _ = let (a,s) = progAffect in evalW a []

let _ = let (b,s) = progFun in evalW b []

(* Répond à la question 2.2.1 et 2.2.2 *)
let _ = assert((let (a,s) = progAffect in evalW a []) =
                 [((Int,['a']),ArithExp 7);((Bool,['b']),BoolExp true);((Fun,['f']),ArithExp 7)])

let _ = assert((let (a,s) = progTest in evalW a []) = [((Fun,['f']), ArithExp 1)])

type config =
  | Inter of programme * state
  | Final of state

(* Permet d'effectuer un pas dans l'exécution du programme p dans l'état s *)
let faire_un_pas = fun p s ->
  match p with
  | Skip -> Final s
  | Declaration (t,p') -> let s' = evalW p s in Final s'
  | AffectI (c, a) -> let s' = evalW p s in Final s'
  | AffectB (c, b) -> let s' = evalW p s in Final s'
  | AffectF (c, f, r) -> let s' = evalW p s in Final s'
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
let _ = let(a,s) = progTest in faire_un_pas a []
let _ = let(a,s) = progAffect in faire_un_pas a []
let _ = assert((let (a,s) = progAffect in faire_un_pas a []) = Inter (Seq (Declaration (Bool, AffectB (['b'], Bco true)),Seq (Declaration (Fun, AffectF (['f'], Skip, ReturnV ['a'])), Skip)), [((Int, ['a']), ArithExp 7)]))

(* Permet d'exécuter entièrement un programme p dans l'état s *)
let rec executer = fun p s ->
  match faire_un_pas p s with
  | Final s -> s
  | Inter (p',s') -> executer p' s' 

(* Permet d'effectuer un pas dans l'exécution du programme p dans l'état s *)
let faire_un_pas_avec_cpt = fun p cpt s ->
  match p with
  | Skip -> (Final s, cpt)
  | Declaration (t, p') -> let s' = evalW p s in (Final s', cpt+1)
  | AffectI (c, a) -> let s' = evalW p s in (Final s', cpt)
  | AffectB (c, b) -> let s' = evalW p s in (Final s', cpt)
  | AffectF (c, f, r) -> let s' = evalW p s in (Final s', cpt)
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

let _ = let (a,s) = progAffect in faire_un_pas_avec_cpt a 0 []

let rec executer_avec_cpt = fun p cpt s ->
  match faire_un_pas_avec_cpt p cpt s with
  | Final s, c -> (s,c)
  | Inter (p',s'), c -> executer_avec_cpt p' c s'

let _ = let (a,s) = progAffect in executer_avec_cpt a 0 []

let rec execution_interactive = fun p s cpt ->
  match faire_un_pas_avec_cpt p s cpt with
  | Final s', c ->
    Printf.printf "Le programme a exécuté %d instruction(s).\n" c;
    s'
  | Inter (p', s'), c ->
    Printf.printf "Etape n° %d. Entrer pour continuer...\n" c;
    ignore (read_line ());
    execution_interactive p' c s'

let _ = let (a,s) = progAffect in execution_interactive a 0 []

let print_executer_result result =
  let rec loop = function res ->
                           match res with
                           | [] -> ()
                           | true :: rest -> print_endline "true"; loop rest
                           | false :: rest -> print_endline "false"; loop rest
  in
  loop result


