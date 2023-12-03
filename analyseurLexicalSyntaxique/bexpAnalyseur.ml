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

let p_suite : char analist = fun l ->
  l |> p_letter -| p_digit

let p_variable : char analist = fun l ->
  l |> p_letter --> (p_suite -| epsilon)

let rec p_nombre : char analist = fun l ->
  l |> p_digit --> (p_nombre -| epsilon)

let p_valeur : char analist = fun l ->
  l |> p_aexp -| p_bool

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
      l |> p_fun -| p_while -| p_if -| p_declaration -| p_affectation -| p_skip and
    p_fun : char analist = fun l ->
      l |> t_fun --> p_variable --> terminal ':' --> p_type --> terminal '{' --> p_programme --> terminal '}' and
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
      l |> (terminal ';' --> p_programme) -| epsilon


let _ = isLetter 'a'
let _ = p_aexp (list_of_string ("5+3"))
let _ = p_declaration (list_of_string ("inta:=5+3"))
let _ = p_programme (list_of_string ("inta:=true;a:=12+13*5;if(true){boola:=2}elseif(false){}else{}"))
