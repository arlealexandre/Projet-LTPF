open Anacomb

(***
Grammaire : 

Variable ::= char · suite
Suite ::= char | digit | '_' | Ɛ

***)


(*********************************** Analist ************************************)
let isLetter =
  fun c ->
  let x = Char.code c in
  (x > 64 && x < 91) || (x > 96 && x < 123) || x = 95

let isDigit =
  fun c ->
  let x = Char.code c - Char.code '0' in (x >= 0 && x <= 9)

let p_digit : char analist = terminal_cond(isDigit)

let p_letter : char analist = terminal_cond (isLetter)

let rec p_suite : char analist = fun l ->
  l |> ((p_letter -| p_digit) --> p_suite) -| epsilon

let p_variable : char analist = fun l ->
  l |> p_letter --> p_suite

let p_nom : char analist =
  fun l ->
  l |> p_letter --> p_suite

(*********************************** ranalist ************************************)

let isLetterR = fun c ->
  let x = Char.code c in
  if((x > 64 && x < 91) || (x > 96 && x < 123) || x = 95)
  then Some(c) else None

let isDigitR = fun c ->
  let x = Char.code c - Char.code '0' in if(x >= 0 && x <= 9) then Some(c) else None

let pr_letter : (char, char) ranalist = terminal_res (isLetterR)
let pr_digit : (char, char) ranalist = terminal_res (isDigitR)

let rec concat = fun (l1 : char list) (l2 : char list) ->
  match l1 with
  | x::q -> x::(concat q l2)
  | _ -> l2

let rec pr_suite (x : char list) : (char list, char) ranalist = fun (l : char list) ->
  l |> (pr_letter ++> fun (x' : char)  -> pr_suite (concat x [x']))
       +| (pr_digit ++> fun i -> pr_suite (concat x [i]))
       +| epsilon_res x

let pr_nom : (char list, char) ranalist = fun l ->
  l |> pr_letter ++> fun x -> pr_suite [x]



