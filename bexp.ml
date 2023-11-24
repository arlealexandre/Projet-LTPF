#use "anacomb.ml";;

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

 ***)

let isConstante =
  fun ( c : char ) ->
  let a = Char.code c - Char.code '0' in (a > (-1)) && (a < 2)

let isVariable =
  fun (c : char) ->
  c= 'a' ||  c = 'b' || c = 'c' || c = 'd'

                                         (************************************** Analist **************************************)

let p_Constante : char analist = terminal_cond (isConstante)
let p_Variable : char analist = terminal_cond (isVariable)

let p_Valeur : char analist =
  fun l ->
  l |> p_Variable -| p_Constante

let rec p_Bexp : char analist =
  fun l ->
  l |> p_Disjonction and
    p_Disjonction : char analist =
      fun l ->
      l |> p_Conjonction --> p_Opt_Disjonction and
    p_Opt_Disjonction : char analist =
      fun l ->
      l |> (terminal '+' --> p_Disjonction) -| epsilon and
    p_Conjonction : char analist =
      fun l ->
      l |> p_Negation --> p_Opt_Conjonction and
    p_Opt_Conjonction : char analist =
      fun l ->
      l |> (terminal '.' --> p_Conjonction) -| epsilon and
    p_Negation : char analist =
      fun l ->
      l |> (terminal '!' --> p_Negation) -| p_Expression and
    p_Expression : char analist =
      fun l ->
      l |> (terminal '(' --> p_Bexp --> terminal ')') -| p_Valeur

(****************************************Ranalist****************************************)

type bexp =
  | Bco of int
  | Bva of char
  | Bneg of bexp
  | Band of bexp * bexp
  | Bor of bexp * bexp

let isConstanteR (c : char) : int option =
  if (c = '1' || c = '0') then Some(Char.code c - Char.code '0') else None

let isVariableR (c: char) : char option =
  if ( c= 'a' ||  c = 'b' || c = 'c' || c = 'd') then Some(c) else None

let pr_Constante : (int, char) ranalist = terminal_res (isConstanteR)

let pr_Variable : (char, char) ranalist = terminal_res (isVariableR)

let pr_Valeur : (bexp, char) ranalist =
  fun l ->
  l |> (pr_Variable ++> fun v->  epsilon_res (Bva v)) +| (pr_Constante ++> fun v -> epsilon_res (Bco v))

let rec pr_Bexp : (bexp, char) ranalist =
  fun l ->
  l |> pr_Disjonction and
    pr_Disjonction : (bexp, char) ranalist =
      fun l ->
      l |> pr_Conjonction ++> fun c -> pr_Opt_Disjonction c and
    pr_Opt_Disjonction (acc : bexp) : (bexp, char) ranalist =
      fun l ->
      l |> (terminal '+' -+> pr_Disjonction ++> fun d ->  epsilon_res (Bor(acc, d))) +| epsilon_res acc and
    pr_Conjonction : (bexp, char) ranalist =
      fun l ->
      l |> pr_Negation ++> fun n -> pr_Opt_Conjonction n and
    pr_Opt_Conjonction (acc : bexp) : (bexp, char) ranalist =
      fun l ->
      l |> (terminal '.' -+> pr_Conjonction ++> fun c -> epsilon_res (Band(acc, c))) +| epsilon_res acc and
    pr_Negation :(bexp, char) ranalist  =
      fun l ->
      l |> (terminal '!' -+> pr_Negation ++> fun n -> epsilon_res (Bneg(n))) +| pr_Expression and
    pr_Expression : (bexp, char) ranalist =
      fun l ->
      l |> (terminal '(' -+> pr_Bexp ++> fun b -> terminal ')' -+> epsilon_res b) +| pr_Valeur

