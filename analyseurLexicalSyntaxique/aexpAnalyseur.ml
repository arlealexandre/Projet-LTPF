#use "../anacomb.ml";;

type aexp =
| Acst of int
| Apl of aexp * aexp
| Amo of aexp * aexp
| Amu of aexp * aexp

(*Grammaire*)
(***
E ::= P
P ::= M SP
M ::= T SM
SP ::= ’+’  M SP | '-' M SP | ε
SM ::= '*' T SM | ε
T := C | '(' E ')'
 ***)


let rec p_aexp : char analist = fun l ->
  l |> p_plus and
    p_plus : char analist = fun l ->
      l |> p_mul --> p_plus_opt and
    p_mul : char analist = fun l ->
      l |> p_number --> p_mul_opt and
    p_plus_opt : char analist = fun l ->
      l |> (terminal '+' --> p_mul --> p_plus_opt) -| (terminal '-' --> p_mul --> p_plus_opt) -| epsilon and
    p_mul_opt : char analist = fun l ->
      l |> (terminal '*' --> p_number --> p_mul_opt) -| epsilon and
    p_number : char analist = fun l ->
      l |> p_c -| (terminal '(' --> p_aexp --> terminal ')'


let rec pr_aexp : (aexp, char) ranalist =
  fun l ->
  l |> pr_Plus and
    pr_Plus : (aexp, char) ranalist =
      fun l ->
      l |> pr_M ++> fun a -> pr_SP a and
    pr_SP (x : aexp)  : (aexp, char) ranalist =
      fun l ->
      l |> (terminal '-' -+> pr_M ++> fun a -> pr_SP (Amo(x,a))) +|(terminal '+' -+> pr_M ++> fun a -> pr_SP (Apl(x,a))) +| (epsilon_res x) and
    pr_M : (aexp, char) ranalist =
      fun l ->
      l |> pr_T1 ++> fun a -> pr_SM a and
    pr_SM (x : aexp)  : (aexp, char) ranalist =
      fun l ->
      l |> (terminal '*' -+> pr_T1 ++> fun a -> pr_SM (Amu(x,a))) +| (epsilon_res x) and
    pr_T1 : (aexp, char) ranalist =
      fun l ->
      l |> (pr_C ++> fun a -> epsilon_res (Acst(a))) +| (terminal '(' -+> pr_E2 ++> fun a -> terminal ')' -+> epsilon_res a)

let expr2 = list_of_string "3-8*3*3"
let bigExpr2 = list_of_string "3+(8-3)*(8+3)"

let _ = let (aex, liste) = pr_E2 expr2 in assert (aex = Amo (Acst 3, Amu (Amu (Acst 8, Acst 3), Acst 3)) && liste = [])
let _ = let (aex, liste) = pr_E2 bigExpr2 in assert (aex = Apl (Acst 3, Amu (Amo (Acst 8, Acst 3), Apl (Acst 8, Acst 3))) && liste = [])
let _ = let (aex, liste) = pr_E1 bigExpr in assert (aex = Apl (Apl (Acst 8, Acst 3), Acst 8) && liste = [])
let _ = let (aex, liste) = pr_E1 bigbigExpr in assert (aex = Apl (Apl (Acst 3, Apl (Acst 8, Acst 3)), Acst 8) && liste = [])
let _ = let (aex, liste) = pr_E1 bigValue in assert (aex = Acst 8 && liste = [])




