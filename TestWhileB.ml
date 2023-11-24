#use "whileB.ml"


(************************************** Analist **************************************)
let _ = assert(p_Affectation (list_of_string "a:=0") = [])
let _ = assert(p_Affectation (list_of_string "b:=1") = [])
let _ = p_Affectation (list_of_string "a=0")

let _ = p_Programme (list_of_string ";;;;")
let _ = p_Programme (list_of_string "a:=1;b:=1;c:=1")
let _ = p_Programme (list_of_string "w(a){};w(a){}")
let _ = p_Programme (list_of_string "w(a){};w(a){}")
let _ = p_Programme (list_of_string "w(a){a:=1}")
let _ = p_Programme (list_of_string "w(a){a:=1;b:=0}")
let _ = p_Programme (list_of_string "w(a){w(b){}}")

let _ = p_Programme (list_of_string "i(a){}{};i(a){}{}")
let _ = p_Programme (list_of_string "i(a){a:=1}{a:=1};i(a){a:=1}{a:=1}")
let _ = p_Programme (list_of_string "i(a){a:=1;b:=0}{a:=1;b:=0};i(a){a:=1;b:=0}{a:=1;b:=0}")

let _ = p_Programme (list_of_string "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}")

let _ = p_Programme (list_of_string "a:=a+b.c+!(1+a)")

let _ = p_Programme (list_of_string "w(!(d.c)+1){};")
let _ = p_Programme (list_of_string "i(a.d.!(a)){}{};")
let _ = p_Programme (list_of_string "a:=1;b:=0;c:=(a+b).a+!b;w(1+0+a){i(!(a+!b)){c:=0;a:=b}{b:=0;c:=a}}")

(****************************************Ranalist****************************************)

let _ = pr_Affectation (list_of_string "a:=0")
let _ = pr_Affectation (list_of_string "b:=1")
let _ = pr_Affectation (list_of_string "b:=a")
let _ = pr_Affectation (list_of_string "b=a")
let _ = pr_Affectation (list_of_string "b:=a;")
let _ = pr_Affectation (list_of_string "g:=a;")

let _ = pr_Programme (list_of_string ";;;;")
let _ = pr_Programme (list_of_string "a:=1;b:=1;c:=1")
let _ = pr_Programme (list_of_string "w(a){};w(a){}")
let _ = pr_Programme (list_of_string "w(a){};w(a){}")
let _ = pr_Programme (list_of_string "w(a){a:=1}")
let _ = pr_Programme (list_of_string "w(a){a:=1;b:=0}")
let _ = pr_Programme (list_of_string "w(a){w(b){}}")

let _ = pr_Programme (list_of_string "i(a){}{};i(a){}{}")
let _ = pr_Programme (list_of_string "i(a){a:=1}{a:=1};i(a){a:=1}{a:=1}")
let _ = pr_Programme (list_of_string "i(a){a:=1;b:=0}{a:=1;b:=0};i(a){a:=1;b:=0}{a:=1;b:=0}")

let _ = pr_Programme (list_of_string "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}")

let _ = pr_Programme (list_of_string "a:=!a+(c.b)")

let _ = pr_Programme (list_of_string "w(!(d.c)+1){};")
let _ = pr_Programme (list_of_string "i(a.d.!(a)){}{};")
let _ = pr_Programme (list_of_string "a:=1;b:=0;c:=(a+b).a+!b;w(1+0+a){i(!(a+!b)){c:=0;a:=b}{b:=0;c:=a}}")
