#use "anacomb.ml";;
#use "variableAnalyseur.ml";;

type aexp =
  | Acst of int
  | Ava of char list
  | Apl of aexp * aexp
  | Amo of aexp * aexp
  | Amu of aexp * aexp
  | Adi of aexp * aexp

(*Grammaire*)
(***
E ::= P
P ::= M SP
M ::= T SM
SP ::= ’+’  M SP | '-' M SP | ε
SM ::= '*' T SM | '/' T SM | ε
T := C | '(' E ')'
 ***)

let isDigit = fun c ->
  let x = Char.code c - Char.code '0' in x >= 0 && x <= 9

let p_digit : char analist = terminal_cond (isDigit)

let rec p_nombre : char analist = fun l ->
  l |> p_digit --> (p_nombre -| epsilon)

let rec p_aexp : char analist = fun l ->
  l |> p_plus and
    p_plus : char analist = fun l ->
      l |> p_mul --> p_plus_opt and
    p_mul : char analist = fun l ->
      l |> p_number --> p_mul_opt and
    p_plus_opt : char analist = fun l ->
      l |> (terminal '+' --> p_mul --> p_plus_opt) -| (terminal '-' --> p_mul --> p_plus_opt) -| epsilon and
    p_mul_opt : char analist = fun l ->
      l |> (terminal '*' --> p_number --> p_mul_opt) -| (terminal '/' --> p_number --> p_mul_opt) -| epsilon and
    p_number : char analist = fun l ->
      l |> p_nombre -| (terminal '(' --> p_aexp --> terminal ')') -| p_nom

let is_Int (c : char) : 'res option =
  let x = Char.code c - Char.code '0' in
  if (x >= 0 && x < 10) then Some(x) else None 

let rec pr_aexp : (aexp, char) ranalist =
  fun l ->
  l |> pr_P and
    pr_P : (aexp, char) ranalist =
      fun l ->
      l |> pr_M ++> fun a -> pr_SP a and
    pr_SP (x : aexp)  : (aexp, char) ranalist =
      fun l ->
      l |> (terminal '-' -+> pr_M ++> fun a -> pr_SP (Amo(x,a))) +| (terminal '+' -+> pr_M ++> fun a -> pr_SP (Apl(x,a))) +| epsilon_res x and
    pr_M : (aexp, char) ranalist =
      fun l ->
      l |> pr_T1 ++> fun a -> pr_SM a and
    pr_SM (x : aexp)  : (aexp, char) ranalist =
      fun l ->
      l |> (terminal '*' -+> pr_T1 ++> fun a -> pr_SM (Amu(x,a))) +| (terminal '/' -+> pr_T1 ++> fun a -> pr_SM (Adi(x,a))) +| epsilon_res x and
    pr_T1 : (aexp, char) ranalist =
      fun l ->
      l |> (pr_int ++> fun i -> pr_int_suite i) +| (terminal '(' -+> pr_aexp ++> fun a -> terminal ')' -+> epsilon_res a)  +| (pr_nom ++> fun nom -> epsilon_res (Ava nom)) and
    pr_int : (int, char) ranalist = fun l ->
      l |> terminal_res (is_Int) and
    pr_int_suite (i : int) : (aexp, char) ranalist = fun l ->
      l |> (terminal_res(is_Int) ++> fun i' -> pr_int_suite (i*10+i')) +| epsilon_res (Acst (i))


