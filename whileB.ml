#use "bexp.ml";;

(***
Grammaire :

Constante ::= 0 | 1
Variable ::= 'a' | 'b' | 'c' | 'd'
Valeur ::= Variable | Constante

Bexp := Disjonction
Disjonction := Conjonction . Opt_Disjonction
Opt_Disjonction := '+' . Disjonction | epsilon
Conjonction := Negation . Opt_Conjonction
Opt_Conjonction := '.' . Conjonction | epsilon
Negation := '!' Negation | Expression
Expression := Valeur | '(' . Bexp . ')'

Programme ::= Instruction·Séparateur
Instruction ::= Affectation | While | If | Skip
Separateur ::= ';'·Programme | Skip
Affectation ::= Variable·':='·Bexp
While ::= 'w'·Condition·'{'·Programme·'}'
If ::= 'i'·Condition·'{'·Programme·'}'
Condition ::= '('·Bexp·')'
Skip ::= Ɛ
 ***)

(************************************** Analist **************************************)


let p_Affectation : char analist =
  fun l ->
  l |> p_Variable --> terminal ':' --> terminal '=' --> p_Bexp

let p_Condition : char analist =
  fun l ->
  l |> terminal '(' --> p_Bexp --> terminal ')'


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

(****************************************Ranalist****************************************)

type whileB =
  | Skip
  | Seq of whileB * whileB
  | Affect of bexp * bexp
  | If of bexp * whileB * whileB
  | While of bexp * whileB

let pr_Affectation : (whileB, char) ranalist =
  fun l ->
  l |> (pr_Variable ++> fun var->  terminal ':' --> terminal '=' -+> pr_Bexp ++> fun valeur -> epsilon_res (Affect (Bva var,valeur)))

let _ = pr_Affectation (list_of_string "a:=0")
let _ = pr_Affectation (list_of_string "b:=1")
let _ = pr_Affectation (list_of_string "b:=a")
let _ = pr_Affectation (list_of_string "b=a")
let _ = pr_Affectation (list_of_string "b:=a;")
let _ = pr_Affectation (list_of_string "g:=a;")

let pr_Condition : (bexp, char) ranalist =
  fun l ->
  l |> ( terminal '(' -+> pr_Bexp ++> fun var -> terminal ')' -+> epsilon_res var)

let rec pr_Programme : (whileB, char) ranalist =
  fun l ->
  l |> pr_Instruction ++> fun exp -> pr_Separateur exp and
     pr_Instruction : (whileB, char) ranalist =
      fun l ->
      l |> pr_Affectation +| pr_While +| pr_If +| pr_Skip and
     pr_Separateur (acc : whileB)  : (whileB, char) ranalist = 
      fun l ->
      l |> (terminal ';' -+> pr_Programme ++> fun p -> epsilon_res (Seq (acc,p))) +| epsilon_res acc and
     pr_If : (whileB, char) ranalist =
      fun l ->      
      l |> terminal 'i' -+> pr_Condition ++> fun cond ->  terminal '{' -+> pr_Programme ++> fun t -> terminal '}'--> terminal '{' -+> pr_Programme ++> fun f -> terminal '}' -+> epsilon_res (If (cond,t,f)) and
     pr_While : (whileB, char) ranalist =
      fun l ->      
      l |> terminal 'w' -+> pr_Condition ++> fun cond ->  terminal '{' -+> pr_Programme ++> fun corps -> terminal '}' -+> epsilon_res (While (cond,corps)) and
     pr_Skip : (whileB, char) ranalist =
       fun l ->
       l |> epsilon_res Skip


