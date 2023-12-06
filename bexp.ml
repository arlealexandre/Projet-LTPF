#use "anacomb.ml";;

(***

Grammaire :

Constante       ::= 0 | 1

Variable        ::= 'a' | 'b' | 'c' | 'd'

Valeur          ::= Variable | Constante

Bexp            ::= Disjonction

Disjonction     ::= Conjonction · Opt_Disjonction

Opt_Disjonction ::= Espace · '+' · Espace · Disjonction | Ɛ

Conjonction     ::= Negation · Opt_Conjonction

Opt_Conjonction ::= Espace · '.' · Espace · Conjonction | Ɛ

Negation        ::= Espace · '!' · Espace · Negation | Expression

Expression      ::= Valeur | '(' · Espace · Bexp · Espace · ')'

 ***)

let isConstante =
  fun ( c : char ) ->
  let a = Char.code c - Char.code '0' in (a > (-1)) && (a < 2)

let isVariable =
  fun (c : char) ->
  c= 'a' ||  c = 'b' || c = 'c' || c = 'd'

(************************************** Analist **************************************)

let p_TypeEspace : char analist =
  fun l ->
  l |> terminal ' ' -| terminal '\t' -| terminal '\n'

let rec p_Espace : char analist =
  fun l ->
  l |> (p_TypeEspace --> p_Espace) -| epsilon

let p_Constante : char analist = terminal_cond (isConstante)
let p_Variable : char analist = terminal_cond (isVariable)
let p_Bool : char analist = fun l ->
  l |> (terminal 't' --> terminal 'r' --> terminal 'u' --> terminal 'e') -| (terminal 'f' --> terminal 'a' --> terminal 'l' --> terminal 's' --> terminal 'e')

let p_Valeur : char analist =
  fun l ->
  l |> p_Variable -| p_Constante -| p_Bool

let rec p_Bexp : char analist =
  fun l ->
  l |> p_Disjonction and
    p_Disjonction : char analist =
      fun l ->
      l |> p_Conjonction --> p_Opt_Disjonction and
    p_Opt_Disjonction : char analist =
      fun l ->
      l |> (p_Espace --> terminal '+' --> p_Espace --> p_Disjonction) -| epsilon and
    p_Conjonction : char analist =
      fun l ->
      l |> p_Negation --> p_Opt_Conjonction and
    p_Opt_Conjonction : char analist =
      fun l ->
      l |> (p_Espace --> terminal '.' --> p_Espace --> p_Conjonction) -| epsilon and
    p_Negation : char analist =
      fun l ->
      l |> (p_Espace --> terminal '!' --> p_Espace --> p_Negation) -| p_Expression and
    p_Expression : char analist =
      fun l ->
      l |> (terminal '(' --> p_Espace --> p_Bexp --> p_Espace --> terminal ')') -| p_Valeur

(****************************************Ranalist****************************************)

type bexp =
  | Bco of bool
  | Bva of char
  | Bneg of bexp
  | Band of bexp * bexp
  | Bor of bexp * bexp

let isConstanteR (c : char) : bool option =
  if (c = '1' || c = '0') then Some(c = '1') else None

let isVariableR (c: char) : char option =
  if ( c= 'a' ||  c = 'b' || c = 'c' || c = 'd') then Some(c) else None

let isBool (b : bool) (c : char) : bool option = if(c = 'e') then Some(b) else None

let pr_Constante : (bool, char) ranalist = terminal_res (isConstanteR)

let pr_Variable : (char, char) ranalist = terminal_res (isVariableR)

let pr_Bool : (bool, char) ranalist = fun l ->
  l |> (terminal 't' --> terminal 'r' --> terminal 'u' -+> terminal_res (isBool true)) +| (terminal 'f' --> terminal 'a' --> terminal 'l' --> terminal 's' -+> terminal_res (isBool false))

let pr_Valeur : (bexp, char) ranalist =
  fun l ->
  l |> (pr_Variable ++> fun v->  epsilon_res (Bva v)) +| (pr_Constante ++> fun v -> epsilon_res (Bco v)) +| (pr_Bool ++> fun v -> epsilon_res (Bco v))

let rec pr_Bexp : (bexp, char) ranalist =
  fun l ->
  l |> pr_Disjonction and
    pr_Disjonction : (bexp, char) ranalist =
      fun l ->
      l |> pr_Conjonction ++> fun c -> pr_Opt_Disjonction c and
    pr_Opt_Disjonction (acc : bexp) : (bexp, char) ranalist =
      fun l ->
      l |> (p_Espace --> terminal '+' --> p_Espace -+> pr_Disjonction ++> fun d ->  epsilon_res (Bor(acc, d))) +| epsilon_res acc and
    pr_Conjonction : (bexp, char) ranalist =
      fun l ->
      l |> pr_Negation ++> fun n -> pr_Opt_Conjonction n and
    pr_Opt_Conjonction (acc : bexp) : (bexp, char) ranalist =
      fun l ->
      l |> (p_Espace --> terminal '.' --> p_Espace -+> pr_Conjonction ++> fun c -> epsilon_res (Band(acc, c))) +| epsilon_res acc and
    pr_Negation :(bexp, char) ranalist  =
      fun l ->
      l |> (p_Espace --> terminal '!' --> p_Espace -+> pr_Negation ++> fun n -> epsilon_res (Bneg(n))) +| pr_Expression and
    pr_Expression : (bexp, char) ranalist =
      fun l ->
      l |> (terminal '(' --> p_Espace -+> pr_Bexp ++> fun b -> p_Espace --> terminal ')' --> p_Espace -+> epsilon_res b) +| pr_Valeur

