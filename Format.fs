module Format

open System.IO
open Basis


// MoscowML has a concat function
let concat = String.concat ""

let mutable linielgd = 72

let udskrivSkaber bredde out ordsek =
    let rec loop ordsek rest =
        match ordsek with
            | Tom -> rest
            | (Str ",") -> out ","; rest-1
            | (Str ".") -> out "."; rest-1
            | (Str "\n") -> out "\n"; bredde
            | (Str w) ->
                    let size w = (String.length w)
                    if rest <> bredde then (* allerede noget på linien *)
                        if (size w) + 1 <= rest then (out " "; out w; rest-size w-1)
                        else (out "\n"; out w; bredde - size w)
                    else if size w <= rest then (out w; rest-size w)
                        else (out "\n"; out w; bredde - (size w))
            | OrdSeq (s1, s2) -> loop s2 (loop s1 rest)
    loop ordsek bredde |> ignore

let udskriv = fun out -> fun ordsek ->  udskrivSkaber linielgd out ordsek

let printer ordsek =
    use os = new StreamWriter("PRN");
    let out : string -> unit  = os.Write 
    udskriv out ordsek; out "\n\n";

let rec strengPrinter = function
    | Str w -> w
    | Tom -> ""
    | OrdSeq (s1, s2) ->
        let combined = $"{strengPrinter s1} {strengPrinter s2}"
        if (String.length combined) > (linielgd / 2)
        then  $"{strengPrinter s1}..."
        else  combined
    
let skaerm ordsek =
    let out = printf "%s"
    udskriv out ordsek
    out "\n\n"

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

let krop indhold = para1 "body" "bgcolor=#fbf2e7" indhold

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
