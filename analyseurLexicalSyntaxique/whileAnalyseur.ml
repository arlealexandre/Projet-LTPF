#use "anacomb.ml";;
#use "aexpAnalyseur.ml";;
#use "bexpAnalyseur.ml";;

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

let t_return : char analist = fun l ->
  l |> terminal 'r' --> terminal 'e' --> terminal 't' --> terminal 'u' --> terminal 'r' --> terminal 'n'

let t_null : char analist = fun l ->
  l |> terminal 'n' --> terminal 'u' --> terminal 'l' --> terminal 'l'

let p_type : char analist = fun l ->
  l |> t_int -| t_bool -| t_fun

let p_return : char analist = fun l ->
  l |> t_return --> (p_variable -| p_aexp -| p_Bexp -| t_null)

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
      l |> p_aexp -| p_Bexp -| (terminal '{' --> p_programme --> p_return --> terminal '}')


let _ = isLetter 'a'
let _ = p_aexp (list_of_string ("5+3"))
let _ = p_declaration (list_of_string ("inta:=5+3"))
let _ = p_if (list_of_string ("if(false){}"))
let _ = p_programme (list_of_string("funf:={returntrue}"))
let _ = p_programme (list_of_string ("inta:=true;aaa:=12+13*5;if(true){boola:=2}elseif(false){}else{funx:={returnnull}}"))

type typeV =
  | Int
  | Bool
  | Fun

type return =
  | Null
  | ReturnI of aexp
  | ReturnB of bexp
  | ReturnV of char list

type programme =
  | Skip
  | Seq of programme * programme
  | AffectI of char list * aexp
  | AffectB of char list * bexp
  | AffectV of char list * char list
  | AffectF of char list * programme * return
  | Declaration of typeV * programme
  | If of bexp * programme * programme
  | While of bexp * programme


let pr_affectI (nom : char list) : (programme, char) ranalist = fun l ->
  l |> pr_aexp ++> fun exp -> epsilon_res (AffectI (nom,exp))

let pr_affectB (nom : char list) : (programme, char) ranalist = fun l ->
  l |> pr_Bexp ++> fun exp -> epsilon_res (AffectB (nom,exp))

let pr_affectV (nom : char list) : (programme,char) ranalist = fun l ->
  l |> (p_True -+> epsilon_res (AffectB (nom, (Bco true))))
       +| (p_False -+> epsilon_res (AffectB (nom, (Bco false))))
       +| (pr_nom ++> fun nom' -> epsilon_res (AffectV (nom,nom')))

let pr_return : (return, char) ranalist = fun l ->
  l |> t_return -+>
         ((t_null -+> epsilon_res (Null))
          +| (p_True -+> epsilon_res (ReturnB (Bco (true))))
          +| (p_False -+> epsilon_res (ReturnB (Bco (false))))
          +| (pr_nom ++> fun nom -> epsilon_res (ReturnV (nom)))
          +| (pr_Bexp ++> fun bexp -> epsilon_res (ReturnB (bexp)))
          +| (pr_aexp ++> fun aexp -> epsilon_res (ReturnI (aexp))))

let verif (res : programme) : (programme,char) ranalist = fun l ->
  match l with
  | [] -> epsilon_res res l
  | x::q when (x = ';') -> epsilon_res res l
  | _ -> raise Echec

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
                      terminal ':' --> terminal '=' -+> pr_affectValeur nom and
    pr_affectValeur (nom : char list) : (programme,char) ranalist = fun l ->
      l |> (pr_affectV nom ++> fun res -> verif res)
           +|(pr_affectB nom ++> fun res -> verif res)
           +| (pr_affectI nom ++> fun res -> verif res)
           +| pr_affectF nom and
    pr_affectF (nom : char list) : (programme, char) ranalist = fun l ->
      l |> terminal '{' -+> pr_programme ++>
             fun prog -> pr_return ++>
                           fun ret -> terminal '}'-+> epsilon_res (AffectF (nom,prog,ret)) and
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
      pr_skip : (programme, char) ranalist = fun l ->
        l |> epsilon_res (Skip) and
      pr_separateur (exp : programme) : (programme, char) ranalist = fun l ->
        l |> (terminal ';' -+> pr_programme ++> fun exp' -> epsilon_res (Seq (exp,exp')))
             +| epsilon_res exp

let _ = pr_programme
          (list_of_string "int a := 5+3*(2-2); if(a = 0){ fun b := { int c := 3; return c}} else if (a = 1) { a := 0 } else { a := 0 }")
let _ = pr_programme (list_of_string "if(true){}else if (false){} else {}")
let _ = pr_programme (list_of_string "int a := a")
let _ = pr_programme (list_of_string "int a := 1; a := a + a")
let _ = pr_programme (list_of_string "int a := 5; fun f := {return a}; a := a + f") 
let _ = pr_if (list_of_string ("if(true){}"))
let _ = pr_programme (list_of_string ("inta:=2;funf:={intb:=true;returnb}"))
let _ = pr_programme
          (list_of_string
             ("inta:=12+5;if(true){funf:={intb:=false;returnnull}}elseif(false){while(b){}}"))
let _ = pr_programme (list_of_string ("fun f := { return 1 }"))
