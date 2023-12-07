#use "../anacomb.ml";;
#use "../bexp.ml";;

(***
Grammaire :

Expression ::= Aexp | Bexp
Valeur ::= Variable | Constante

Variable ::= nom . suite
Suite ::= char | digit | epsilon

Booleen ::= 'True' | 'False'

Bexp ::= Disjonction
Disjonction := Conjonction . Opt_Disjonction
Opt_Disjonction := '+' . Disjonction | epsilon
Conjonction := Negation . Opt_Conjonction
Opt_Conjonction := '.' . Conjonction | epsilon
Negation := '!' Negation | Expression
Expression := Booleen | '(' . Bexp . ')'

Aexp ::= PlusMoins
PlusMoins ::= Mul_Div opt_plus_moins
Mul_Div ::= Term opt_mul_div 
opt_plus_moins ::= ’+’  Mul_Div opt_plus_moins | '-' Mul_Div opt_plus_moins | ε
opt_mul_div ::= '*' Term opt_mul_div | '/' Term opt_mul_div | ε
Term := Constante | '(' Aexp ')'
Constante ::= Chiffre opt_chiffre

Programme ::= Instruction·Séparateur
Instruction ::= Declaration | Affectation | While | If | Fonction | Skip
Separateur ::= ';'·Programme | Skip
Declaration ::= token . Affectation
Affectation ::= Variable·':=' · Exp
While ::= 'w'·Condition·'{'·Programme·'}'
If ::= 'if'·Condition·'{'·Programme·'}' . if_opt
if_opt ::= 'else' If | 'else' . { . Programme . '}' | epsilon
Condition ::= '('·Bexp·')'
Skip ::= Ɛ
 ***)
let t_while : char analist = fun l ->
  l |> terminal 'w' --> terminal 'h' --> terminal 'i' --> terminal 'l' --> terminal 'e'

let t_if : char analist = fun l ->
  l |> terminal 'i' --> terminal 'f'

let t_else : char analist = fun l ->
  l |> terminal 'e' --> terminal 'l' --> terminal 's' --> terminal 'e'

let t_int : char analist = fun l ->
  l |> terminal 'i' --> terminal 'n' --> terminal 't'

let t_bool : char analist = fun l ->
  l |> terminal 'b' --> terminal 'o' --> terminal 'o' --> terminal 'l'

let t_fun : char analist = fun l ->
  l |> terminal 'f' --> terminal 'u' --> terminal 'n'

let p_bool : char analist = fun l ->
  l |> (terminal 't' --> terminal 'r' --> terminal 'u' --> terminal 'e')
       -| (terminal 'f' --> terminal 'a' --> terminal 'l' --> terminal 's' --> terminal 'e')

let p_type : char analist = fun l ->
  l |> t_int -| t_bool -| t_fun

let isLetter = fun c ->
  (Char.code c > 64 && Char.code c < 91) || (Char.code c > 96 && Char.code c < 123)

let isDigit = fun c ->
  let x = Char.code c - Char.code '0' in x >= 0 && x <= 9

let p_letter : char analist = terminal_cond (isLetter)
let p_digit : char analist = terminal_cond (isDigit)

let rec p_suite : char analist = fun l ->
  l |> ((p_letter -| p_digit) --> p_suite) -| epsilon

let p_variable : char analist = fun l ->
  l |> p_letter --> p_suite

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
      l |> (terminal '*' --> p_number --> p_mul_opt) -| epsilon and
    p_number : char analist = fun l ->
      l |> p_nombre -| (terminal '(' --> p_aexp --> terminal ')')


let rec p_programme : char analist = fun l ->
  l |> p_instruction --> p_separateur and
    p_instruction : char analist = fun l ->
      l |> p_while -| p_if -| p_declaration -| p_affectation -| p_skip and
    p_declaration : char analist = fun l ->
      l |> p_type --> p_affectation and
    p_affectation : char analist = fun l ->
      l |> p_variable --> terminal ':' --> terminal '=' --> p_valeur and
    p_while : char analist = fun l ->
      l |> t_while --> terminal '(' --> p_Bexp --> terminal ')' --> terminal '{' --> p_programme --> terminal '}' and
    p_if : char analist = fun l ->
      l |> t_if --> terminal '(' --> p_Bexp --> terminal ')' --> terminal '{' --> p_programme --> terminal '}' --> p_opt_if and
    p_opt_if : char analist = fun l ->
      l |> (t_else --> p_if --> p_opt_if) -| (t_else --> terminal '{' --> p_programme  --> terminal '}') -| epsilon and
    p_skip : char analist = fun l ->
      l |> epsilon and
    p_separateur : char analist = fun l ->
      l |> (terminal ';' --> p_programme) -| epsilon and
    p_valeur : char analist = fun l ->
      l |> p_aexp -| p_Bexp -| (terminal '{' --> p_programme --> terminal '}')


let _ = isLetter 'a'
let _ = p_aexp (list_of_string ("5+3"))
let _ = p_declaration (list_of_string ("inta:=5+3"))
let _ = p_if (list_of_string ("if(false){}"))
let _ = p_programme (list_of_string("funf:={}"))
let _ = p_programme (list_of_string ("inta:=true;aaa:=12+13*5;if(true){boola:=2}elseif(false){}else{funx:={}}"))

type typeV =
  | Int
  | Bool
  | Fun

type aexp =
| Acst of int
| Apl of aexp * aexp
| Amo of aexp * aexp
| Amu of aexp * aexp

type programme =
  | Skip
  | Seq of programme * programme
  | AffectI of char list * aexp
  | AffectB of char list * bexp
  | AffectF of char list * programme
  | Declaration of typeV * programme
  | If of bexp * programme * programme
  | While of bexp * programme

type dictionnaire =
  | VInt of typeV * int
  | VBool of typeV * bool
  | VFun of typeV * programme


let rec concat = fun (l1 : char list) (l2 : char list) ->
  match l1 with
  | x::q -> x::(concat q l2)
  | _ -> l2

let isLetterR = fun c ->
  if((Char.code c > 64 && Char.code c < 91) || (Char.code c > 96 && Char.code c < 123))
  then Some(c) else None

let isDigitR = fun c ->
  let x = Char.code c - Char.code '0' in if(x >= 0 && x <= 9) then Some(c) else None

let is_Int (c : char) : 'res option =
  let x = Char.code c - Char.code '0' in
  if (x >= 0 && x < 10) then Some(x) else None

let pr_letter : (char, char) ranalist = terminal_res (isLetterR)
let pr_digit : (char, char) ranalist = terminal_res (isDigitR)

let rec pr_suite (x : char list) : (char list, char) ranalist = fun (l : char list) ->
  l |> (pr_letter ++> fun (x' : char)  -> pr_suite (concat x [x']))
       +| (pr_digit ++> fun i -> pr_suite (concat x [i]))
       +| epsilon_res x

let pr_nom : (char list, char) ranalist = fun l ->
  l |> pr_letter ++> fun x -> pr_suite [x]

let rec pr_aexp : (aexp, char) ranalist =
  fun l ->
  l |> pr_P and
    pr_P : (aexp, char) ranalist =
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
      l |> (pr_int ++> fun i -> pr_int_suite i) +| (terminal '(' -+> pr_aexp ++> fun a -> terminal ')' -+> epsilon_res a) and
    pr_int : (int, char) ranalist = fun l ->
      l |> terminal_res (is_Int) and
    pr_int_suite (i : int) : (aexp, char) ranalist = fun l ->
      l |> (terminal_res(is_Int) ++> fun i' -> pr_int_suite (i*10+i')) +| epsilon_res (Acst (i))

let _ = pr_aexp (list_of_string ("12+3"))
let _ = pr_aexp (list_of_string ("20001+3-5*2"))


let pr_affectI (nom : char list) : (programme, char) ranalist = fun l ->
  l |> pr_aexp ++> fun exp -> epsilon_res (AffectI (nom,exp))

let pr_affectB (nom : char list) : (programme, char) ranalist = fun l ->
  l |> pr_Bexp ++> fun exp -> epsilon_res (AffectB (nom,exp))
  

let rec pr_programme : (programme, char) ranalist = fun l ->
  l |> pr_instruction ++> fun exp -> pr_separateur exp and
    pr_instruction : (programme, char) ranalist = fun l ->
      l |> pr_while +| pr_if +| pr_declaration +| pr_affectation +| pr_skip and
    pr_declaration : (programme, char) ranalist = fun l ->
      l |> (t_int -+> pr_affectation ++> fun exp -> epsilon_res (Declaration (Int, exp)))
           +| (t_bool -+> pr_affectation ++> fun exp -> epsilon_res (Declaration (Bool, exp)))
           +| (t_fun -+> pr_affectation ++> fun exp -> epsilon_res (Declaration (Fun, exp)))
    and
      pr_affectation : (programme, char) ranalist = fun l ->
      l |> pr_nom ++> fun nom ->
                      terminal ':' --> terminal '='
                      -+> (pr_affectI nom +| pr_affectB nom +| pr_affectF nom) and
      pr_if : (programme, char) ranalist = fun l ->
      l |> t_if --> terminal '(' -+> pr_Bexp ++>
             fun bexp -> terminal ')' --> terminal '{' -+> pr_programme ++>
                           fun prog -> terminal '}' -+> pr_opt_if bexp prog and
      pr_opt_if (bexp : bexp) (prog : programme) : (programme, char) ranalist = fun l ->
        l |> (t_else -+> ( (pr_if ++> fun prog' -> epsilon_res (If (bexp,prog,prog') ) ) 
                         +| (terminal '{' -+> pr_programme ++> fun prog' -> terminal '}' -+> epsilon_res (If (bexp,prog,prog')))))
                         +| (epsilon_res (If (bexp,prog,Skip))) and
      pr_while : (programme, char) ranalist = fun l ->
        l |> t_while --> terminal '(' -+> pr_Bexp ++> fun bexp -> terminal ')' --> terminal '{' -+> pr_programme ++> fun prog -> terminal '}' -+> epsilon_res (While (bexp,prog)) and
      pr_affectF (nom : char list) : (programme, char) ranalist = fun l ->
        l |> terminal '{' -+> pr_programme ++> fun prog -> terminal '}' -+> epsilon_res (AffectF (nom,prog)) and
      pr_skip : (programme, char) ranalist = fun l ->
        l |> epsilon_res (Skip) and
      pr_separateur (exp : programme) : (programme, char) ranalist = fun l ->
        l |> (terminal ';' -+> pr_programme ++> fun exp' -> epsilon_res (Seq (exp,exp')))
             +| epsilon_res exp

let _ = pr_if (list_of_string ("if(true){}"))
let _ = pr_programme
          (list_of_string
             ("inta:=12+5;if(true){funf:={intb:=false}}elseif(false){while(b){}}"))
