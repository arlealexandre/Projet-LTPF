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
  | Function of programme * return
  | VarName of char list

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

let rec verifType (t:typeV) (v:valeur) (s : state) : bool =
  match t,v with
  | (Int, ArithExp (x)) -> true
  | (Bool, BoolExp (x)) -> true
  | (Fun, _) -> true
  | (_,VarName x) -> true
  | (_,_) -> false
and
 create (s:state) ((t,v):variable) (n:valeur) : state =
  let rec f = fun (s':state) ((t',v'):variable) (n':valeur) : state ->
                                                          match s' with
                                                          | [] -> if (verifType t n s) then
                                                                    [((t,v),n)]
                                                                  else
                                                                    raise (ExceptionType "mauvais type")
                                                          | x::q -> x::(f q (t',v') n') in
                                                          
  try let _ = (get v s) in raise (VarAlreadyExists "var existe déjà") with
  | VarNotFound (i) -> f s (t,v) n
  | VarAlreadyExists (i) -> raise (VarAlreadyExists "var existe déjà")
and
update (s:state) (v:char list) (n:valeur): state =
  match s with
  | [] -> raise (VarNotFound "var not found")
  | (var,vale)::q when (contains v var) ->
     let (t,v') = var in
     if (verifType t n s) then
       (var,n)::q
     else
       raise (ExceptionType "mauvais type")
  | (var,vale)::q -> (var,vale)::(update q v n)
and
evalV = fun (v : char list) (s:state) ->
  let res = get v s in
  match res with
  | VarName v' ->  evalV v' s
  | Function (f,r) -> let s' = evalW f s in evalR r s'
  | _ -> res
and
 evalA = fun (a:aexp) (s:state) ->
  match a with
  | Acst(i) -> i
  | Ava(v) -> let res = evalV v s in
              (match res with
               | ArithExp x -> x
               | _ -> raise (ExceptionType "mauvais type"))
  | Apl(a1,a2) -> (evalA a1 s) + (evalA a2 s) 
  | Amo(a1,a2) -> (evalA a1 s) - (evalA a2 s)
  | Amu(a1,a2) -> (evalA a1 s) * (evalA a2 s)
  | Adi(a1,a2) -> (evalA a1 s) / (evalA a2 s)
and
evalB = fun (b:bexp) (s:state) ->
  match b with
  | Bco(x) -> x
  | Bva(v) -> let res = evalV v s in
              (match res with
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
and
evalR = fun (r:return) (s:state) ->
  match r with
  | Null -> Null
  | ReturnI (a) -> ArithExp (evalA a s)
  | ReturnB (b) -> BoolExp (evalB b s)
  | ReturnV (v) -> evalV v s
and
evalW = fun w s ->
  match w with
  | Skip -> s
  | Seq (i1, i2) -> let s' = evalW i1 s in evalW i2 s'
  | Declaration (t, p) -> (match p with
                           | AffectI (c, a) -> create s (t,c) (ArithExp (evalA a s))
                           | AffectB (c, b) -> create s (t,c) (BoolExp (evalB b s))
                           | AffectF (c, f, r) -> create s (t,c) (Function (f,r))
                           | AffectV (c, v) -> create s (t,c) (VarName v)
                           | _ -> raise (BadWriting "code mal écrit"))
                            
  | AffectI (c, a) -> update s c (ArithExp (evalA a s))
  | AffectV (c, v) -> update s c (VarName v)
  | AffectB (c, b) -> update s c (BoolExp (evalB b s))
  | AffectF (c, f, r) -> update s c (Function(f,r))
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
let progFun = pr_programme (list_of_string "int a := 1; fun f := { a := 3; return a }; a := a + f; a := a + f")

let progTest = pr_programme (list_of_string "int a := 1; int b := a; int c := b; a := b + c")

let _ = let (a,s) = progTest in evalW a []

let _ = let (a,s) = progAffect in evalW a []

let _ = let (b,s) = progFun in evalW b []

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
  | AffectV (c, v) -> let s' = evalW p s in Final s'
  | AffectF (c, f, r) -> let s' = evalW p s in Final s'
  | Seq (i1, i2) -> let s' = (evalW i1 s) in Inter (i2, s')
  | If (c, i1, i2) ->
     if evalB c s then
       let s' = (evalW i1 s) in Final s'
     else
       let s' = (evalW i2 s) in Final s'
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
  | Declaration (t, p') -> let s' = evalW p s in (Final s', cpt)
  | AffectI (c, a) -> let s' = evalW p s in (Final s', cpt+1)
  | AffectB (c, b) -> let s' = evalW p s in (Final s', cpt+1)
  | AffectV (c, v) -> let s' = evalW p s in (Final s', cpt+1)
  | AffectF (c, f, r) -> let s' = evalW p s in (Final s', cpt+1)
  | Seq (i1, i2) -> let s' = (evalW i1 s) in (Inter (i2, s'), cpt+1)
  | If (c, i1, i2) ->
     if evalB c s then
       let s' = (evalW i1 s) in (Final s', cpt)
     else
       let s' = (evalW i2 s) in (Final s', cpt)
  | While (c, i1) ->
     if evalB c s then
       let s' = evalW i1 s in (Inter ((While (c, i1)), s'), cpt)
     else
      (Final s, cpt+1)
      
let rec executer_avec_cpt = fun p cpt s ->
  match faire_un_pas_avec_cpt p cpt s with
  | Final s, c -> (s,c)
  | Inter (p',s'), c -> executer_avec_cpt p' c s'

let string_of_typeV = function
  | Int -> "int"
  | Bool -> "bool"
  | Fun -> "fun"

let rec string_of_valeur = function
  | Null -> "Null"
  | BoolExp b -> string_of_bool b
  | ArithExp i -> string_of_int i
  | Function (f,r) -> "return " 
  | VarName v ->  String.of_seq (List.to_seq v)

let print_interactive_result result =
  Printf.printf "[";
  let rec loop = function
    | [] -> Printf.printf "]\n"
    | ((t, l), vale)::rest ->
      if rest=[] then
        (Printf.printf "%s %s := %s" (string_of_typeV t) (String.of_seq (List.to_seq l)) (string_of_valeur vale); loop rest)
      else
        (Printf.printf "%s %s := %s; " (string_of_typeV t) (String.of_seq (List.to_seq l)) (string_of_valeur vale); loop rest)
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


