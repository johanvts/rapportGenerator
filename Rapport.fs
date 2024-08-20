module Rapport

open System
open Basis

// &&& already in used as OrdSek operator as && is reserved in F#
// Hence ml source operator &&& is @@@ in F# version, and && is &&&
let (@@@) s1 s2 () = s1 () &&& s2 ()     (* konkatenering af to   *)
let (&&*) ss () =  List.fold (fun res s -> res &&& (s ())) Tom ss
let (>>>) p s () = if ssh p then s() else Tom (* valg eller ikke af en *)

let (||*) ss () = vaelgfra ss ()      (* lige valg blandt alle *)

let private vaelgNyUdenGentagelserInstans = 
    (* lige valg blandt strenge, men undgå at vælge samme for ofte *)
    // i.e. der skal være mindst 30 andre valg før en gentagelse, hvis muligt.
    let mutable valgNummer = 0
    let  sidstegang = Array.create 117 -999999
    let rec vaelgny ss =
        valgNummer <- valgNummer + 1
        let mulig = vaelgfra ss
        // Looks like Polyhash.hash produced only postive hashes
        let klasse = abs (hash mulig % 117)
        printfn "%s -> %i" mulig klasse
        printfn "%i" valgNummer
        if sidstegang.[klasse] + 30 > valgNummer then
            vaelgny ss
        else
            sidstegang.[klasse] <- valgNummer
            Str mulig
    fun ss () -> vaelgny ss

// This level of custom operator is hard. Replacing ||$* here: 
let vaelgNyUdenGentagelser  = vaelgNyUdenGentagelserInstans

let (|||) s1 s2 () = vaelgfra [|s1; s2|] () (* lige valg blandt to   *)

// Again $ is not allowed in F#. Going with s
let s s () = Str s

(* afsnitsoverskrifter *)
type private item = int * string * ordsek (* niveau * anker og titel *)
type private afsnit = A of item * afsnit list
type private partial = afsnit list

let mutable private niveau = 0 (* afsnittets niveau: 0, 1, 2, ... *)
let mutable private overskrifter = ([] : partial)

let private overskrift niv s = 
    match niv with
        | 0 -> Format.overskrift0 s
        | 1 -> Format.overskrift1 s
        | _ -> failwith $"overskrift niveau {niv} ikke understøttet."  

let rec private slutunder (partial : partial) : partial = 
    let rec loop remaining sidste res =
        match remaining with
        | [] -> res
        | (A((niv, anker, ovs), [])) :: rest when niv = sidste ->
            loop rest sidste (A((niv, anker, ovs), res) :: res)
        | a :: rest -> loop rest sidste (a :: res)
    loop partial niveau []

let begyndafsnit () = niveau <- niveau + 1

let slutafsnit () = 
    overskrifter <- slutunder overskrifter
    niveau <- niveau - 1

let nytafsnit (ovsk : ordsek) =
    let (anker, ovsk') = Format.lavAnker ovsk
    overskrifter <- A((niveau, anker, ovsk), []) :: overskrifter
    overskrift niveau ovsk'

let indhold () =
    let rec afsnit (A((niv, anker, s), uafs)) =
        Format.li (Format.href anker s) &&& underafsnit uafs
    and underafsnit = function
        | [] -> Tom
        | uafs -> Format.ul (List.fold (fun res uaf -> res &&& afsnit uaf) Tom uafs)

    overskrift 0 (Str "IndholdsFortegnelse")
    &&& underafsnit (List.rev overskrifter)
    &&& Format.streg ()
        
let generator = "http://www.matfys.kvl.dk/~sestoft/center.html"

let kolofon =
    (Format.streg 
    @@@ (nytafsnit << s"Kolofon"))
    @@@ s"Dette er forslag nummer" @@@ Basis.skrivseed
//    @@@ s"produceret den" @@@ Format.dato
    @@@ s"af den fuldautomatiske rapportgenerator"
    @@@ s"ved" 
//    @@@ Format.href generator o s generator
    @@@ s"."
    @@@ s"Rapporten er trykt på genbrugspapir" 
    @@@ s"og overholder gældende EU-normer for klarhed og relevans."

let forfattere () =
    nytafsnit (Str "Appendiks: Arbejdsgruppens medlemmer")
    &&& Str "Arbejdsgruppen bag rapporten bestod af"
//    &&& Format.ul (mksek (5 + terning 5) 
//                        (Format.li << Format.begyndelse << Navne.person))
    &&& Format.afsnit ()
    &&& Str "Endvidere rettes en tak til" 
//    &&& Navne.person () &&& Str "og" &&& Navne.person () 
    &&& Str "for konstruktiv kritik." 

 
//let nominal () = Led.nominal None |> fun (_,_,s) -> s

let adverbial = 
    0.3 >>> vaelgNyUdenGentagelser
             [| "af omveje"; "aldrig"; "blot"; "delvis"; 
              "dybest set"; "effektivt"; "eventuelt"; "fortrinsvis"; 
              "først og fremmest"; "generelt";
              "gradvis"; "ikke"; "ikke nødvendigvis"; 
              "indadtil"; "indirekte"; "i det lange løb"; 
              "i ringe grad";
              "isoleret set";
              "kun"; "kun sjældent"; "langt oftere"; "løst sagt";
              "med tiden"; "midlertidigt"; "muligvis";
              "måske"; "måske ikke"; "noget indirekte"; "nok ikke"; 
              "ofte"; "partielt"; "potentielt"; 
              "ret typisk"; "ret utvetydigt";  
              "sjældent"; "så at sige"; 
              "temmelig entydigt"; "tendentielt"; 
              "utvivlsomt"; "kun vanskeligt"; "væsentligst" |]
(*
let verbPraesIndAkt = 
    ||$* [| "accentuerer"; "afmystificerer"; "angÃ¥r"; "belyser"; "begrunder"; 
           "berÃ¸rer"; "beskriver"; 
           "besvÃ¦rliggÃ¸r"; "effektiviserer"; "eksternaliserer"; 
           "erstatter";
           "forandrer"; "foregriber"; "fornyer";
           "forstÃ¦rker"; "fortrÃ¦nger"; "fremmer"; "hÃ¦mmer";
           "implicerer"; "involverer"; "karakteriserer"; 
           "kendetegner"; "klarlÃ¦gger";
           "komplicerer";
           "modarbejder"; "moderniserer"; "modsvarer"; 
           "omdefinerer";
           "problematiserer"; "profilerer"; "pÃ¥virker"; 
           "reducerer";
           "styrker"; "stÃ¸tter"; "svÃ¦kker"; "udnytter"; "udvikler";
           "understÃ¸tter"; "vedrÃ¸rer" |]

let konjunktion =
    ||$* [| "da"; "da"; "eftersom"; "eftersom"; "fordi"; "forudsat"; 
           "hvis"; "ikke mindst fordi"; "mens"; "netop fordi";
           "nÃ¥r"; "nÃ¥r blot"; "pÃ¥ trods af at"; 
           "selvom"; "selvom"; "skÃ¸nt"; "sÃ¥fremt" |]

let ledsaetning =
    nominal &&& adverbial &&& verbPraesIndAkt &&& nominal

let ledsaetning2 =
    verbPraesIndAkt &&& nominal &&& adverbial &&& nominal

let hovedsaetning =
    nominal &&& verbPraesIndAkt &&& adverbial &&& nominal

let konstatering = 
    (||* ( [|
        fun () -> Str "det er" &&& (||$* [| "beklageligt"; "bevist"; "forstÃ¥eligt"; 
                                          "klart"; "indiskutabelt"; 
                                          "nÃ¸dvendigt"; "oplagt"; "pÃ¥faldende"; "velkendt" |]),
        (||$* [| "anerkendte"; "enkelte"; "de fleste"; "danske"; "isolerede"; 
                 "omhyggelige";  
                 "samtlige"; "trovÃ¦rdige"; "uafhÃ¦ngige"; 
                 "udenlandske"; "visse"; "vores" |])
              &&& (||$* [| "analyser"; "forskere"; "iagttagere"; "resultater";
                         "studier"; "undersÃ¸gelser" |])
              &&& (||$* [| "antyder"; "demonstrerer"; "fastslÃ¥r"; 
                         "lader formode"; "pÃ¥peger"; "viser" |])
              ]))
    &&& fun () -> Str "at" 
    &&& ledsaetning
    ||| hovedsaetning)
    &&& (||* [|
        fun () -> Str "," &&& konjunktion &&& ledsaetning
                &&& (fun () -> if ssh 0.05 then Str (", og " + konjunktion()) &&& ledsaetning else Tom),
        fun () -> Str "," &&& ((Str "der") ||| (Str "som")) &&& (if ssh 0.5 then ||$* [| "af denne Ã¥rsag"; "derfor"; "fÃ¸lgelig"; 
                                       "samtidig"; "sidelÃ¸bende"; 
                                       "sÃ¥ledes"; "trods dette" |] &&& adverbial &&& verbPraesIndAkt &&& nominal else Tom)
        ,fun () -> if ssh 0.3 then Str "." &&& Str "For det fÃ¸rste fordi" &&& ledsaetning &&& Str "," &&& Str "og for det andet fordi" &&& ledsaetning else Tom
        ]))

let konstatering = fun () -> Format.begyndelse (konstatering ()) && Str "."

let raesonnement =
    konjunktion
    &&& ledsaetning
    &&& Str ","
    &&& ((||$* [| "bÃ¸r"; "kan"; "mÃ¥"; "skal" |]) &&& Str "det"
         &&& (||$* [| "antages"; "betones"; "betvivles"; "forudsÃ¦ttes";
                     "konstateres"; "pointeres"; 
                     "pÃ¥peges"; "understreges" |])
         ||| (||$* [| "bÃ¸r"; "kan"; "mÃ¥"; "skal" |]) 
             &&& (||$* [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |]) 
             &&& (||$* [| "acceptere"; "anerkende"; "antage"; "beklage"; 
                        "sikre"; "forudsÃ¦tte"; "konstatere" |]))
    &&& Str "at" 
    &&& (ledsaetning
         ||| Str "dette" &&& adverbial &&& verbPraesIndAkt &&& nominal)

let raesonnement = fun () -> Format.begyndelse (raesonnement ()) && Str "."

let konsekvens =
    (||$* [|Str "det" 
            &&& (||$* [| "fÃ¸lger"; "indses"; "konkluderes"; "ses" |]),
          (||$* [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |])
             &&& (||$* [| "konkluderer"; "ser"; "slutter" |]),
          Str "der" &&& Str "gÃ¦lder"
         ])
    &&& (||$* [| "altsÃ¥"; "da"; "derfor"; "endda"; "endvidere"; 
                 "nu"; "straks"; "sÃ¥ledes"; "tillige";
                 "uden videre"; "ret umiddelbart"; "umiddelbart" |])
    &&& Str "," &&& Str "at"
    &&& ledsaetning
    &&& (fun () -> if ssh 0.7 then Str "," &&& Str "og" &&& Str "at" &&& ledsaetning else Tom)
    ||| (||$* [| "altsÃ¥"; "af disse grunde"; "derfor"; "klart nok";
               "fÃ¸lgelig"; "sÃ¥ledes" |])
        &&& ledsaetning2

let konsekvens = fun () -> Format.begyndelse (konsekvens ()) && Str "."

let mutable billednr = 0
let skrivnr url = 
    billednr <- billednr + 1
    url + "?billed=" + strseed() + "-" + billednr.ToString()

let billede url () = 
    Format.afsnit () && Format.center (Format.billede (skrivnr url))

let mkafsnit () =
    Format.afsnit ()
    && (fun () -> nytafsnit (Format.begyndelse (nominal ())))
    && konstatering ()
    && mksek (3 + terning 5) (||* [| konstatering; raesonnement; konsekvens |])
    && (fun () -> if ssh 0.25 then billede "lagkage"() ||| billede "kurver"() else Tom)

let baggrund () = 
    (fun () -> nytafsnit (Str "Baggrund"); begyndafsnit ())
    && mksek (2 + terning 3) mkafsnit
    && fun () -> slutafsnit()

let titelnom () =                            
    let (form, koen, nom, _) = Substant.vaelg (Some (ubs, Flt))
    Led.adj form koen && nom

let centernavn = fun () -> titelnom () && Str "og" && titelnom ()

let samarbejde =
    Format.afsnit
    &&& Str "Centret vil vÃ¦re en oplagt partner for det nyligt" 
    &&& Str "foreslÃ¥ede center for" &&& centernavn () &&& Str "," 
    &&& Str "ligesom der bÃ¸r kunne opnÃ¥s en frugtbar symbiose med"
    &&& Str "centret for" &&& centernavn () &&& Str "."

let oprettelse center =
    fun () -> Format.begyndelse (
        ( ||$* [|"de anfÃ¸rte"; "ovenstÃ¥ende"; "de opregnede"]
          &&& ||$* [|"argumenter"; "betragtninger"; "forhold"; "grunde";
                    "konstateringer"; "overvejelser"; 
                    "rÃ¦sonnementer"]
          &&& ||$* [|"fÃ¸rer"; "leder"]
          &&& (fun () -> if ssh 0.5 then ||$* [|"logisk"; "nÃ¸dvendigvis"; "os"; "uomgÃ¦ngeligt"] else Tom)
          &&& (fun () -> if ssh 0.3 then Str "frem" else Tom)
          &&& Str "til den konklusion at der" 
          &&& ||$* [|"er behov for"; "mÃ¥ oprettes"; "bÃ¸r etableres"]
          &&& Str "et virtuelt center for" &&& center )
          &&& Str ".")

let anbefaling center () = 
    (fun () -> nytafsnit (Str "Anbefaling"); begyndafsnit ())
    && oprettelse (fun () -> center) ()
    && samarbejde ()
    && fun () -> slutafsnit()

let diskussion () = 
    (fun () -> nytafsnit (Str "Diskussion"); begyndafsnit ())
    && mksek (2 + terning 3) mkafsnit
    && fun () -> slutafsnit()

let rapport () = 
    let center = centernavn ()
    let tekst = (baggrund 
                 &&& diskussion 
                 &&& (fun () -> anbefaling center ())
                 &&& forfattere
                 &&& kolofon) ()
    Format.html (Str "Forslag til virtuelt center for" && center)
                (indhold () && tekst)



*)                
