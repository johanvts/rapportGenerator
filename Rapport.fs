module Rapport

open System
open Basis
open LagkageGenerator

// &&& already in used as OrdSek operator as && is reserved in F#
// Hence ml source operator &&& is @@@ in F# version, and && is &&&
let (@@@) s1 s2 () = s1 () &&& s2 ()     (* konkatenering af to   *)
let (&&*) ss () =  List.fold (fun res s -> res &&& (s ())) Tom ss
let (>>>) p s () = if ssh p then s() else Tom (* valg eller ikke af en *)

let vaelgLige ss () = vaelgfra ss ()      (* lige valg blandt alle *)

let private vaelgNyUdenGentagelserInstans = 
    (* lige valg blandt strenge, men undgÃ¥ at vÃ¦lge samme for ofte *)
    // i.e. der skal vÃ¦re mindst 30 andre valg fÃ¸r en gentagelse, hvis muligt.
    let mutable valgNummer = 0
    let  sidstegang = Array.create 117 -999999
    let rec vaelgny ss =
        valgNummer <- valgNummer + 1
        let mulig = vaelgfra ss
        // Looks like Polyhash.hash produced only postive hashes
        let klasse = abs (hash mulig % 117)
        if sidstegang.[klasse] + 30 > valgNummer then
            vaelgny ss
        else
            sidstegang.[klasse] <- valgNummer
            Str mulig
    fun ss () -> vaelgny ss

// This level of custom operator is hard. Replacing ||$* here: 
let vaelgNyUg  = vaelgNyUdenGentagelserInstans

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
        | _ -> failwith $"overskrift niveau {niv} ikke understÃ¸ttet."  

let rec private slutunder (partial : partial) : partial = 
    let rec loop remaining sidste res =
        match remaining with
        | [] -> res
        | ((A((niv, anker, ovs), [])) as afs) :: rest ->
            if niv = sidste then
                loop rest sidste (afs :: res)
            else
                (A((niv, anker, ovs), res) :: rest)
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
    @@@ s"Rapporten er trykt pÃ¥ genbrugspapir" 
    @@@ s"og overholder gÃ¦ldende EU-normer for klarhed og relevans."

let forfattere () =
    nytafsnit (Str "Appendiks: Arbejdsgruppens medlemmer")
    &&& Str "Arbejdsgruppen bag rapporten bestod af"
    &&& Format.ul (mksek (5 + terning 5) 
                        (Format.li << Format.begyndelse << Navne.person))
    &&& Format.afsnit ()
    &&& Str "Endvidere rettes en tak til" 
    &&& Navne.person () &&& Str "og" &&& Navne.person () 
    &&& Str "for konstruktiv kritik." 

 
let nominal () = (Led.nominal None).Item3

let adverbial = 
    0.3 >>> vaelgNyUg
            [|"af omveje"; "aldrig"; "blot"; "delvis"; 
              "dybest set"; "effektivt"; "eventuelt"; "fortrinsvis"; 
              "fÃ¸rst og fremmest"; "generelt";
              "gradvis"; "ikke"; "ikke nÃ¸dvendigvis"; 
              "indadtil"; "indirekte"; "i det lange lÃ¸b"; 
              "i ringe grad";
              "isoleret set";
              "kun"; "kun sjÃ¦ldent"; "langt oftere"; "lÃ¸st sagt";
              "med tiden"; "midlertidigt"; "muligvis";
              "mÃ¥ske"; "mÃ¥ske ikke"; "noget indirekte"; "nok ikke"; 
              "ofte"; "partielt"; "potentielt"; 
              "ret typisk"; "ret utvetydigt";  
              "sjÃ¦ldent"; "sÃ¥ at sige"; 
              "temmelig entydigt"; "tendentielt"; 
              "utvivlsomt"; "kun vanskeligt"; "vÃ¦sentligst"|]
              
let verbPraesIndAkt = 
    vaelgNyUg [|"accentuerer"; "afmystificerer"; "angÃ¥r"; "belyser"; "begrunder"; 
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
           "understÃ¸tter"; "vedrÃ¸rer"|]
           
let konjunktion =
    vaelgNyUg [|"da"; "da"; "eftersom"; "eftersom"; "fordi"; "forudsat"; 
           "hvis"; "ikke mindst fordi"; "mens"; "netop fordi";
           "nÃ¥r"; "nÃ¥r blot"; "pÃ¥ trods af at"; 
           "selvom"; "selvom"; "skÃ¸nt"; "sÃ¥fremt"|]

let ledsaetning =
    nominal @@@ adverbial @@@ verbPraesIndAkt @@@ nominal

let ledsaetning2 =
    verbPraesIndAkt @@@ nominal @@@ adverbial @@@ nominal

let hovedsaetning =
    nominal @@@ verbPraesIndAkt @@@ adverbial @@@ nominal

let konstateringUdenFormat = 
    (vaelgLige [| s"det er" @@@ (vaelgNyUg [| "beklageligt"; "bevist"; "forstÃ¥eligt"; 
                                          "klart"; "indiskutabelt"; 
                                          "nÃ¸dvendigt"; "oplagt"; "pÃ¥faldende"; "velkendt" |]);
                vaelgNyUg [| "anerkendte"; "enkelte"; "de fleste"; "danske"; "isolerede"; 
                           "omhyggelige";  
                           "samtlige"; "trovÃ¦rdige"; "uafhÃ¦ngige"; 
                           "udenlandske"; "visse"; "vores" |]
              @@@ vaelgNyUg [| "analyser"; "forskere"; "iagttagere"; "resultater";
                             "studier"; "undersÃ¸gelser" |]
              @@@ vaelgNyUg [| "antyder"; "demonstrerer"; "fastslÃ¥r"; 
                             "lader formode"; "pÃ¥peger"; "viser" |]
              |]
     @@@ s"at" 
     @@@ ledsaetning
     ||| hovedsaetning)
     @@@ vaelgLige [| s"," @@@ konjunktion @@@ ledsaetning
                           @@@ (0.05 >>>
                                (s"," @@@ s"og " @@@ konjunktion @@@ ledsaetning));
                      s","
                           @@@ (s"der" ||| s"som")
                           @@@ (0.5 >>> vaelgNyUg [| "af denne Ã¥rsag"; "derfor"; "fÃ¸lgelig"; 
                                       "samtidig"; "sidelÃ¸bende"; 
                                       "sÃ¥ledes"; "trods dette" |])
                           @@@ adverbial @@@ verbPraesIndAkt @@@ nominal;
                    0.3 >>>(s"."
                            @@@ s"For det fÃ¸rste fordi" @@@ ledsaetning
                            @@@ s","
                            @@@ s"og for det andet fordi" @@@ ledsaetning)
                 |]

let konstatering = Format.begyndelse << konstateringUdenFormat @@@ s"."

let raesonnementUdenFormat =
    konjunktion
    @@@ ledsaetning
    @@@ s ","
    @@@ ((vaelgNyUg [| "bÃ¸r"; "kan"; "mÃ¥"; "skal" |]) @@@ s "det"
         @@@ (vaelgNyUg [| "antages"; "betones"; "betvivles"; "forudsÃ¦ttes";
                     "konstateres"; "pointeres"; 
                     "pÃ¥peges"; "understreges" |])
         ||| (vaelgNyUg [| "bÃ¸r"; "kan"; "mÃ¥"; "skal" |]) 
             @@@ (vaelgNyUg [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |]) 
             @@@ (vaelgNyUg [| "acceptere"; "anerkende"; "antage"; "beklage"; 
                        "sikre"; "forudsÃ¦tte"; "konstatere" |]))
    @@@ s "at" 
    @@@ (ledsaetning
         ||| s "dette" @@@ adverbial @@@ verbPraesIndAkt @@@ nominal)

let raesonnement = Format.begyndelse << raesonnementUdenFormat @@@ s "."

let konsekvensUdenFormat =
    (vaelgLige [|s "det" 
            @@@ (vaelgNyUg [| "fÃ¸lger"; "indses"; "konkluderes"; "ses" |]);
                vaelgNyUg [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |]
                @@@ (vaelgNyUg [| "konkluderer"; "ser"; "slutter" |]);
          s "der" @@@ s "gÃ¦lder"
         |])
    @@@ (vaelgNyUg [| "altsÃ¥"; "da"; "derfor"; "endda"; "endvidere"; 
                 "nu"; "straks"; "sÃ¥ledes"; "tillige";
                 "uden videre"; "ret umiddelbart"; "umiddelbart" |])
    @@@ s "," @@@ s "at"
    @@@ ledsaetning
    @@@ (0.7 >>>  s "," @@@ s "og" @@@ s "at" @@@ ledsaetning)
    ||| (vaelgNyUg [| "altsÃ¥"; "af disse grunde"; "derfor"; "klart nok";
               "fÃ¸lgelig"; "sÃ¥ledes" |])
        @@@ ledsaetning2

let konsekvens = Format.begyndelse << konsekvensUdenFormat @@@ s "."

(* Krumspring for at sikre at friske grafer genereres: *)

let mutable private billednr = 0

let billede url () =
    let skrivnr url = 
        billednr <- billednr + 1
        url + "?billed=" + strseed() + "-" + billednr.ToString()
    Format.afsnit () &&& Format.center (Format.billede (skrivnr url))

let mkafsnit () =
    Format.afsnit ()
    &&& nytafsnit (Format.begyndelse (nominal ()))
    &&& konstatering ()
    &&& mksek (3 + terning 5) (vaelgLige [| konstatering; raesonnement; konsekvens |])
//    &&& (0.25 >>> (billede "lagkage" ||| billede "kurver")) ()
    &&& (0.25 >>> (fun _ -> Format.center (Str (opretLagkage 400.0 (Seq.toList (Seq.init (Math.Max(2, terning 8)) (fun _ -> nominal ())))  ((float)(terning 3)))))) ()

let ``before`` e1 e2 =
    let result = e1 ()
    e2 () |> ignore
    result

let baggrund () = 
    before (fun _ -> nytafsnit (Str "Baggrund")) begyndafsnit
    &&& before (fun _ -> mksek (2 + terning 3) mkafsnit) slutafsnit


let titelnom () =                            
    let (form, koen, nom, _) = Substant.vaelg (Some (Ubs, Flt))
    Led.adj form koen &&& nom

let centernavn =  titelnom @@@ vaelgNyUg [|"og"; "samt"|] @@@ titelnom

let samarbejde =
    Format.afsnit
    @@@ s "Centret vil vÃ¦re en oplagt partner for det nyligt" 
    @@@ s "foreslÃ¥ede center for" @@@ centernavn @@@ s "," 
    @@@ s "ligesom der bÃ¸r kunne opnÃ¥s en frugtbar symbiose med"
    @@@ s "centret for" @@@ centernavn @@@ s "."

let rec oprettelse center =
    Format.begyndelse <<
         (vaelgNyUg [|"de anfÃ¸rte"; "ovenstÃ¥ende"; "de opregnede"|]
          @@@ vaelgNyUg [|"argumenter"; "betragtninger"; "forhold"; "grunde";
                    "konstateringer"; "overvejelser"; 
                    "rÃ¦sonnementer"|]
          @@@ vaelgNyUg [|"fÃ¸rer"; "leder"|]
          @@@ (0.5 >>> vaelgNyUg [|"logisk"; "nÃ¸dvendigvis"; "os"; "uomgÃ¦ngeligt"|])
          @@@ (0.3 >>> s "frem")
          @@@ s "til den konklusion at der" 
          @@@ vaelgNyUg [|"er behov for"; "mÃ¥ oprettes"; "bÃ¸r etableres"|]
          @@@ s "et virtuelt center for" @@@ (fun _ ->  center)
          @@@ s ".")

let anbefaling center () = 
    before (fun _ -> nytafsnit (Str "Anbefaling")) begyndafsnit 
    &&& oprettelse center ()
    &&& before samarbejde slutafsnit

let diskussion () = 
    before   (fun _ -> nytafsnit (Str "Diskussion")) begyndafsnit
    &&&
    before (fun _ -> mksek (2 + terning 3) mkafsnit) slutafsnit

let rapport () = 
    let center = centernavn ()
    let tekst = (baggrund 
                       @@@ diskussion 
                       @@@ anbefaling center
                       @@@ forfattere
                       @@@ kolofon) ()
    Format.html (Str "Forslag til virtuelt center for" &&& center)
                (indhold () &&& tekst)
