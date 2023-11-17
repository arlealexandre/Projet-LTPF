#use "anacomb.ml";;

(***
Constante ::= 0 | 1
Variable ::= 'a' | 'b' | 'c' | 'd'
Valeur ::= Variable | Constante

Programme ::= Instruction·Séparateur
Instruction ::= Affectation | While | If | Skip
Separateur ::= ';'·Programme | Skip
Affectation ::= Variable·':='·Valeur
While ::= 'w'·Condition·'{'·Programme·'}'
If ::= 'i'·Condition·'{'·Programme·'}'
Condition ::= '('·Variable·')'
Skip ::= Ɛ
***)


let isConstante =
  fun ( c : char ) ->
  (Char.code c - Char.code '0' < 2)

let isVariable =
  fun (c : char) ->
  c= 'a' ||  c = 'b' || c = 'c' || c = 'd'

(************************************** Analist **************************************)

let p_Constante : char analist = terminal_cond (isConstante)

let p_Variable : char analist = terminal_cond (isVariable)

let p_Valeur : char analist =
  fun l ->
  l |> p_Variable -| p_Constante

let p_Affectation : char analist =
  fun l ->
  l |> p_Variable --> terminal ':' --> terminal '=' --> p_Valeur

let _ = assert(p_Affectation (list_of_string "a:=0") = [])
let _ = assert(p_Affectation (list_of_string "b:=1") = [])
let _ = p_Affectation (list_of_string "a=0")

let p_Condition : char analist =
  fun l ->
  l |> terminal '(' --> p_Variable --> terminal ')'


let rec p_Programme : char analist =
  fun l ->
  l |> p_Instruction --> p_Separateur and
    p_Instruction : char analist =
      fun l ->
      l |> p_Affectation -| p_While -| p_If -| p_Skip and 
    p_Separateur : char analist = 
      fun l ->
      l |> terminal ';' --> p_Programme -| epsilon and
    p_If : char analist =
      fun l ->      
      l |> terminal 'i' --> p_Condition --> terminal '{' --> p_Programme--> terminal '}'--> terminal '{' --> p_Programme --> terminal '}' and
    p_While : char analist =
      fun l ->
      l |> terminal 'w' --> p_Condition --> terminal '{' --> p_Programme -->  terminal '}' and
    p_Skip : char analist  =
      fun l ->
      l |> epsilon

let _ = p_Programme (list_of_string ";;;;")
let _ = p_Programme (list_of_string "a:=1;b:=1;c:=1")
let _ = p_Programme (list_of_string "w(a){};w(a){}")
let _ = p_Programme (list_of_string "w(a){};w(a){}")
let _ = p_Programme (list_of_string "w(a){a:=1}")
let _ = p_Programme (list_of_string "w(a){a:=1;b:=0}")
let _ = p_Programme (list_of_string "w(a){w(b){}}")

let _ = p_Programme (list_of_string "i(a){}{};i(a){}{}")
let _ = p_Programme (list_of_string "i(a){a:=1}{a:=1};i(a){a:=1}{a:=1}")
let _ = p_Programme (list_of_string "i(a){a:=1;b:=0}{a:=1;b:=0};i(a){a:=1;b:=0}{a:=1;b:=0}")

let _ = p_Programme (list_of_string "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}")


                    (***********************************************Ranalist****************************************)
type whileBm =
  | Aco of int
  | Ava of char
  | Skip
  | Seq of whileBm * whileBm
  | Affect of whileBm * whileBm
  | If of whileBm * whileBm *whileBm
  | While of whileBm * whileBm

let isConstanteR (c : char) : int option =
  if (Char.code c - Char.code '0' < 2) then Some(Char.code c - Char.code '0') else None

let isVariable (c: char) : char option =
  if ( c= 'a' ||  c = 'b' || c = 'c' || c = 'd') then Some(c) else None

let pr_Constante : (int, char) ranalist = terminal_res (isConstanteR)

let _ = pr_Constante (list_of_string "0")
let _ = pr_Constante (list_of_string "1")
let _ = pr_Constante (list_of_string "2")

let pr_Variable : (char, char) ranalist = terminal_res (isVariable)
let _ = pr_Variable (list_of_string "a")
let _ = pr_Variable (list_of_string "b")
let _ = pr_Variable (list_of_string "c")
let _ = pr_Variable (list_of_string "d")
let _ = pr_Variable (list_of_string "e")

let pr_Valeur : (whileBm, char) ranalist =
  fun l ->
  l |> (pr_Variable ++> fun v->  epsilon_res (Ava v)) +| (pr_Constante ++> fun v -> epsilon_res (Aco v))

let pr_Affectation : (whileBm, char) ranalist =
  fun l ->
  l |> (pr_Variable ++> fun var->  terminal ':' --> terminal '=' -+> pr_Valeur ++> fun valeur -> epsilon_res (Affect (Ava var,valeur)))

let _ = pr_Affectation (list_of_string "a:=0")
let _ = pr_Affectation (list_of_string "b:=1")
let _ = pr_Affectation (list_of_string "b:=a")
let _ = pr_Affectation (list_of_string "b=a")
let _ = pr_Affectation (list_of_string "b:=a;")
let _ = pr_Affectation (list_of_string "g:=a;")

let pr_Condition : (whileBm, char) ranalist =
  fun l ->
  l |> ( terminal '(' -+> pr_Variable ++> fun var -> terminal ')' -+> epsilon_res (Ava var))

let _ = pr_Condition (list_of_string "(a)")
let _ = pr_Condition (list_of_string "(b)")
let _ = pr_Condition (list_of_string "(g)")
let _ = pr_Condition (list_of_string ")a)")
let _ = pr_Condition (list_of_string "(a(")
let _ = pr_Condition (list_of_string "(a)je sais pas")




let rec pr_Programme : (whileBm, char) ranalist =
  fun l ->
  l |> pr_Instruction ++> fun exp -> pr_Separateur exp and
     pr_Instruction : (whileBm, char) ranalist =
      fun l ->
      l |> pr_Affectation +| pr_While +| pr_If +| pr_Skip and
     pr_Separateur (acc : whileBm)  : (whileBm, char) ranalist = 
      fun l ->
      l |> (terminal ';' -+> pr_Programme ++> fun p -> epsilon_res (Seq (acc,p))) +| epsilon_res acc and
     pr_If : (whileBm, char) ranalist =
      fun l ->      
      l |> terminal 'i' -+> pr_Condition ++> fun cond ->  terminal '{' -+> pr_Programme ++> fun t -> terminal '}'--> terminal '{' -+> pr_Programme ++> fun f -> terminal '}' -+> epsilon_res (If (cond,t,f)) and
     pr_While : (whileBm, char) ranalist =
      fun l ->      
      l |> terminal 'w' -+> pr_Condition ++> fun cond ->  terminal '{' -+> pr_Programme ++> fun corps -> terminal '}' -+> epsilon_res (While (cond,corps)) and
     pr_Skip : (whileBm, char) ranalist =
       fun l ->
       l |> epsilon_res Skip

let rec pr_test : (whileBm, char) ranalist =
  fun l ->
  l |> pr_Ins ++> fun exp -> pr_Sep exp and
    pr_Ins : (whileBm, char) ranalist =
      fun l ->
      l |> pr_Affectation and
     pr_Sep (acc : whileBm)  : (whileBm, char) ranalist = 
      fun l ->
      l |> (terminal ';' -+> pr_test ++> fun p -> epsilon_res (Seq (acc,p))) +| epsilon_res acc

let _ = pr_test (list_of_string "a:=0;b:=1;c:=1")


let _ = pr_Programme (list_of_string "")
let _ = pr_Programme (list_of_string ";")

let _ = pr_Programme (list_of_string ";;;;")
let _ = pr_Programme (list_of_string "a:=0")
let _ = pr_Programme (list_of_string "a:=0;")

let _ = pr_Programme (list_of_string "a:=1;b:=1;c:=1")
let _ = pr_Programme (list_of_string "w(a){};w(a){}")
let _ = pr_Programme (list_of_string "w(a){};w(a){}")
let _ = pr_Programme (list_of_string "w(a){a:=1}")
let _ = pr_Programme (list_of_string "w(a){a:=1;b:=0}")
let _ = pr_Programme (list_of_string "w(a){w(b){}}")

let _ = pr_Programme (list_of_string "i(a){}{};i(a){}{}")
let _ = pr_Programme (list_of_string "i(a){a:=1}{a:=1};i(a){a:=1}{a:=1}")
let _ = pr_Programme (list_of_string "i(a){a:=1;b:=0}{a:=1;b:=0};i(a){a:=1;b:=0}{a:=1;b:=0}")

let _ = pr_Programme (list_of_string "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}")
     
