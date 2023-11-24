#use "bexp.ml";;

(************************************** Analist **************************************)

let _ = p_Bexp (list_of_string "1")
let _ = p_Bexp (list_of_string "0")
let _ = p_Bexp (list_of_string "2")
let _ = p_Bexp (list_of_string "a")

let _ = p_Bexp (list_of_string "1.a")
let _ = p_Bexp (list_of_string "b+0")
let _ = p_Bexp (list_of_string "!c")
let _ = p_Bexp (list_of_string "a+b.!c")
let _ = p_Bexp (list_of_string "(1)")

let _ = p_Bexp (list_of_string "(1+b).!(a.c+0)")


(****************************************Ranalist****************************************)


let _ = pr_Constante (list_of_string "0")
let _ = pr_Constante (list_of_string "1")
let _ = pr_Constante (list_of_string "2")

let _ = pr_Variable (list_of_string "a")
let _ = pr_Variable (list_of_string "b")
let _ = pr_Variable (list_of_string "c")
let _ = pr_Variable (list_of_string "d")
let _ = pr_Variable (list_of_string "e")

let _ = pr_Bexp (list_of_string "1")
let _ = pr_Bexp (list_of_string "0")
let _ = pr_Bexp (list_of_string "2")
let _ = pr_Bexp (list_of_string "a")

let _ = pr_Bexp (list_of_string "1.a")
let _ = pr_Bexp (list_of_string "b+0")
let _ = pr_Bexp (list_of_string "a+b")
let _ = pr_Bexp (list_of_string "a+0")
let _ = pr_Bexp (list_of_string "b+a")
let _ = pr_Bexp (list_of_string "b+1")
let _ = pr_Bexp (list_of_string "b+0")
let _ = pr_Bexp (list_of_string "c+0")
let _ = pr_Bexp (list_of_string "d+0")


let _ = pr_Bexp (list_of_string "!c")
let _ = pr_Bexp (list_of_string "!!!!c")

let _ = pr_Bexp (list_of_string "a+b.!c")
let _ = pr_Bexp (list_of_string "(1)")
let _ = pr_Bexp (list_of_string "(1+b).!(a.c+0)")
let _ = pr_Bexp (list_of_string "(a+b).c")
let _ = pr_Bexp (list_of_string "a+b.c")
let _ = pr_Bexp (list_of_string "a+(b.c)")
let _ = pr_Bexp (list_of_string "!a+!(b.c)")
