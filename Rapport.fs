module Rapport

open System
open Basis

// &&& already in used as OrdSek operator as && is reserved in F#
// Hence ml source operator &&& is @@@ in F# version, and && is &&&
let (@@@) s1 s2 () = s1 () &&& s2 ()     (* konkatenering af to   *)
let (&&*) ss () =  List.fold (fun res s -> res &&& (s ())) Tom ss
let (>>>) p s () = if ssh p then s() else Tom (* valg eller ikke af en *)

let vaelgLige ss () = vaelgfra ss ()      (* lige valg blandt alle *)

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

 
let nominal () = (Led.nominal None).Item3

let adverbial = 
    0.3 >>> vaelgNyUg
            [|"af omveje"; "aldrig"; "blot"; "delvis"; 
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
              "utvivlsomt"; "kun vanskeligt"; "væsentligst"|]
              
let verbPraesIndAkt = 
    vaelgNyUg [|"accentuerer"; "afmystificerer"; "angår"; "belyser"; "begrunder"; 
           "berører"; "beskriver";
           "besværliggør"; "effektiviserer"; "eksternaliserer"; 
           "erstatter";
           "forandrer"; "foregriber"; "fornyer";
           "forstærker"; "fortrænger"; "fremmer"; "hæmmer";
           "implicerer"; "involverer"; "karakteriserer"; 
           "kendetegner"; "klarlægger";
           "komplicerer";
           "modarbejder"; "moderniserer"; "modsvarer"; 
           "omdefinerer";
           "problematiserer"; "profilerer"; "påvirker"; 
           "reducerer";
           "styrker"; "støtter"; "svækker"; "udnytter"; "udvikler";
           "understøtter"; "vedrører"|]
           
let konjunktion =
    vaelgNyUg [|"da"; "da"; "eftersom"; "eftersom"; "fordi"; "forudsat"; 
           "hvis"; "ikke mindst fordi"; "mens"; "netop fordi";
           "når"; "når blot"; "på trods af at"; 
           "selvom"; "selvom"; "skønt"; "såfremt"|]

let ledsaetning =
    nominal @@@ adverbial @@@ verbPraesIndAkt @@@ nominal

let ledsaetning2 =
    verbPraesIndAkt @@@ nominal @@@ adverbial @@@ nominal

let hovedsaetning =
    nominal @@@ verbPraesIndAkt @@@ adverbial @@@ nominal

let konstateringUdenFormat = 
    (vaelgLige [| s"det er" @@@ (vaelgNyUg [| "beklageligt"; "bevist"; "forståeligt"; 
                                          "klart"; "indiskutabelt"; 
                                          "nødvendigt"; "oplagt"; "påfaldende"; "velkendt" |]);
                vaelgNyUg [| "anerkendte"; "enkelte"; "de fleste"; "danske"; "isolerede"; 
                           "omhyggelige";  
                           "samtlige"; "troværdige"; "uafhængige"; 
                           "udenlandske"; "visse"; "vores" |]
              @@@ vaelgNyUg [| "analyser"; "forskere"; "iagttagere"; "resultater";
                             "studier"; "undersøgelser" |]
              @@@ vaelgNyUg [| "antyder"; "demonstrerer"; "fastslår"; 
                             "lader formode"; "påpeger"; "viser" |]
              |]
     @@@ s"at" 
     @@@ ledsaetning
     ||| hovedsaetning)
     @@@ vaelgLige [| s"," @@@ konjunktion @@@ ledsaetning
                           @@@ (0.05 >>>
                                (s"," @@@ s"og " @@@ konjunktion @@@ ledsaetning));
                      s","
                           @@@ (s"der" ||| s"som")
                           @@@ (0.5 >>> vaelgNyUg [| "af denne årsag"; "derfor"; "følgelig"; 
                                       "samtidig"; "sideløbende"; 
                                       "således"; "trods dette" |])
                           @@@ adverbial @@@ verbPraesIndAkt @@@ nominal;
                    0.3 >>>(s"."
                            @@@ s"For det første fordi" @@@ ledsaetning
                            @@@ s","
                            @@@ s"og for det andet fordi" @@@ ledsaetning)
                 |]

let konstatering = Format.begyndelse << konstateringUdenFormat @@@ s"."

let raesonnementUdenFormat =
    konjunktion
    @@@ ledsaetning
    @@@ s ","
    @@@ ((vaelgNyUg [| "bør"; "kan"; "må"; "skal" |]) @@@ s "det"
         @@@ (vaelgNyUg [| "antages"; "betones"; "betvivles"; "forudsættes";
                     "konstateres"; "pointeres"; 
                     "påpeges"; "understreges" |])
         ||| (vaelgNyUg [| "bør"; "kan"; "må"; "skal" |]) 
             @@@ (vaelgNyUg [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |]) 
             @@@ (vaelgNyUg [| "acceptere"; "anerkende"; "antage"; "beklage"; 
                        "sikre"; "forudsætte"; "konstatere" |]))
    @@@ s "at" 
    @@@ (ledsaetning
         ||| s "dette" @@@ adverbial @@@ verbPraesIndAkt @@@ nominal)

let raesonnement = Format.begyndelse << raesonnementUdenFormat @@@ s "."

let konsekvensUdenFormat =
    (vaelgLige [|s "det" 
            @@@ (vaelgNyUg [| "følger"; "indses"; "konkluderes"; "ses" |]);
                vaelgNyUg [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |]
                @@@ (vaelgNyUg [| "konkluderer"; "ser"; "slutter" |]);
          s "der" @@@ s "gælder"
         |])
    @@@ (vaelgNyUg [| "altså"; "da"; "derfor"; "endda"; "endvidere"; 
                 "nu"; "straks"; "således"; "tillige";
                 "uden videre"; "ret umiddelbart"; "umiddelbart" |])
    @@@ s "," @@@ s "at"
    @@@ ledsaetning
    @@@ (0.7 >>>  s "," @@@ s "og" @@@ s "at" @@@ ledsaetning)
    ||| (vaelgNyUg [| "altså"; "af disse grunde"; "derfor"; "klart nok";
               "følgelig"; "således" |])
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
    &&& (0.25 >>> (billede "lagkage" ||| billede "kurver")) ()

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
    @@@ s "Centret vil være en oplagt partner for det nyligt" 
    @@@ s "foreslåede center for" @@@ centernavn @@@ s "," 
    @@@ s "ligesom der bør kunne opnås en frugtbar symbiose med"
    @@@ s "centret for" @@@ centernavn @@@ s "."

let rec oprettelse center =
    Format.begyndelse <<
         (vaelgNyUg [|"de anførte"; "ovenstående"; "de opregnede"|]
          @@@ vaelgNyUg [|"argumenter"; "betragtninger"; "forhold"; "grunde";
                    "konstateringer"; "overvejelser"; 
                    "ræsonnementer"|]
          @@@ vaelgNyUg [|"fører"; "leder"|]
          @@@ (0.5 >>> vaelgNyUg [|"logisk"; "nødvendigvis"; "os"; "uomgængeligt"|])
          @@@ (0.3 >>> s "frem")
          @@@ s "til den konklusion at der" 
          @@@ vaelgNyUg [|"er behov for"; "må oprettes"; "bør etableres"|]
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
