module Format

open Basis

// MoscowML has a concat function
let concat = String.concat ""

let rec begyndelse = function
  | Tom -> Tom  
  | Str w -> Str (w.[0].ToString().ToUpper() + w.Substring(1))
  | OrdSeq (s1, s2) ->
      match begyndelse s1 with
       | Tom -> begyndelse s2
       | s1' -> s1' &&& s2

(* Html *)

let para0 art indhold =
    Str (concat ["<"; art; ">"])
    &&& indhold
    &&& Str (concat ["</"; art; ">"])

let para1 art parameter indhold =
    Str (concat ["<"; art; " "; parameter; ">"])
    &&& indhold
    &&& Str (concat ["</"; art; ">"])

let href link indhold =
    para1 "a" (concat ["href=\""; link; "\""]) indhold

let ul indhold = Str "\n" &&& para0 "ul" indhold &&& Str "\n"
let li indhold = Str "\n" &&& para0 "li" indhold

let afsnit () = Str "\n" &&& Str "<p>"

let streg  () = Str "\n" &&& Str "<hr>"

let hoved s = para0 "head" (para0 "title" s)

let krop indhold = para1 "body" "bgcolor=#FFFFFF" indhold

let overskrift0 indhold = Str "\n" &&& para0 "H1" indhold &&& Str "\n"
let overskrift1 indhold = Str "\n" &&& para0 "H2" indhold &&& Str "\n"

let center = para0 "center"

let billede url =
    Str (concat ["<img src=\""; url; "\" align=center>"])

(* ankre af form anker1, anker2, ... *)
let mutable private next = 0
let private  nytAnker () =
    next <- next + 1
    "anker" + next.ToString()

let lavAnker s = 
    let anker = nytAnker ()
    ("#" + anker, 
     para1 "a" (concat ["name=\""; anker; "\""]) s)

let  html centernavn indhold = 
    Str "Content-type: text/html" 
    &&& Str "\n" &&& Str "\n"
    &&& para0 "html" (hoved centernavn
                      &&& streg ()
                      &&& para0 "h1" centernavn
                      &&& streg ()
                      &&& krop indhold)
