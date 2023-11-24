#use "whileB.ml";;


let rec get (x:int) (s:bool list) : bool =
  match x,s with
  | 0, v::s -> v
  | i , x::l1 -> get (i-1) l1
  | _, [] -> false

let tab = true :: false :: true :: false :: []

let _ = get (1) tab


let rec update (s:bool list) (v:int) (n:bool): bool list =
  match v,s with
  | 0   , a :: l1 -> n :: l1
  | 0   , nil     -> n :: nil
  | i, a :: l1 -> a :: (update l1 (i-1) n)
  | i, nil     -> false :: (update nil (i-1) n)

let _ = update tab 200 true


let state = true :: false :: true ::[]

let rec evalB = fun b s ->
  match b with
  | Bco(x) -> x
  | Bva(v) -> (get (Char.code v - Char.code 'a') s)
  | Bneg(n) -> not (evalB n s)
  | Band(x1, x2) -> (evalB x1 s) && (evalB x2 s)
  | Bor(x1, x2) -> (evalB x1 s) || (evalB x2 s)

let _ = evalB (Bva('b')) state
let _ = evalB (Bneg(Bva('a'))) state
let _ = evalB (Band(Bva('a'), Bva('b'))) state
let _ = evalB (Bor(Bva('a'), Bva('b'))) state

let rec evalW =
  fun w s ->
  match w with
  | Skip -> s
  | Seq(i1,i2) -> evalW i1 s |> evalW i2
  | Affect(c, b2) -> update s (Char.code c - Char.code 'a')  (evalB b2)
