module Rapport

open System
open Basis

// &&& already in used as OrdSek operator as && is reserved in F#
// Hence ml source operator &&& is @@@ in F# version, and && is &&&
let (@@@) s1 s2 = s1 () &&& s2 ()     (* konkatenering af to   *)
let (&&*) ss () =  List.fold (fun res s -> res &&& (s ())) Tom ss
let (>>>) p s () = if ssh p then s() else Tom (* valg eller ikke af en *)

let (||*) ss () = vaelgfra ss ()      (* lige valg blandt alle *)

let private vaelgNyUdenGentagelserInstans = 
    (* lige valg blandt strenge, men undg� at v�lge samme for ofte *)
    // i.e. der skal v�re mindst 30 andre valg f�r en gentagelse, hvis muligt.
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
        | _ -> failwith $"overskrift niveau {niv} ikke underst�ttet."  

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
(*
let rec afsnit (A((niv, anker, s), uafs)) =
    Format.li (Format.href anker s) && underafsnit uafs
and underafsnit uafs =
    match uafs with
    | [] -> Tom
    | _ -> Format.ul (List.foldBack (fun uaf res -> OrSeq(res, afsnit uaf)) uafs Tom)

let indhold () =
    overskrift 0 (Str "Indholdsfortegnelse") && underafsnit (List.rev overskrifter) && Format.streg ()

let generator = "http://www.matfys.kvl.dk/~sestoft/center.html"

let kolofon =
    Format.streg 
    &&& (fun () -> nytafsnit (fun() -> Str "Kolofon"))
    &&& fun () -> Str ("Dette er forslag nummer " + skrivseed() + " produceret den " + Format.dato () + " af den fuldautomatiske rapportgenerator ved " + Format.href generator generator + ". Rapporten er trykt på genbrugspapir og overholder gældende EU-normer for klarhed og relevans.")

let forfattere () =
    nytafsnit (Str "Appendiks: Arbejdsgruppens medlemmer")
    && Str "Arbejdsgruppen bag rapporten bestod af"
    && Format.ul (mksek (5 + terning 5) 
                        (fun () -> Format.li (Format.begyndelse (Navne.person()))))
    && Format.afsnit ()
    && Str "Endvidere rettes en tak til" 
    && Navne.person () && Str "og" && Navne.person () 
    && Str "for konstruktiv kritik." 

let nominal () = Led.nominal None |> fun (_,_,s) -> s

let adverbial = 
    0.3 >>> (||$* [| "af omveje"; "aldrig"; "blot"; "delvis"; 
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
                    "utvivlsomt"; "kun vanskeligt"; "væsentligst" |])

let verbPraesIndAkt = 
    ||$* [| "accentuerer"; "afmystificerer"; "angår"; "belyser"; "begrunder"; 
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
           "understøtter"; "vedrører" |]

let konjunktion =
    ||$* [| "da"; "da"; "eftersom"; "eftersom"; "fordi"; "forudsat"; 
           "hvis"; "ikke mindst fordi"; "mens"; "netop fordi";
           "når"; "når blot"; "på trods af at"; 
           "selvom"; "selvom"; "skønt"; "såfremt" |]

let ledsaetning =
    nominal &&& adverbial &&& verbPraesIndAkt &&& nominal

let ledsaetning2 =
    verbPraesIndAkt &&& nominal &&& adverbial &&& nominal

let hovedsaetning =
    nominal &&& verbPraesIndAkt &&& adverbial &&& nominal

let konstatering = 
    (||* ( [|
        fun () -> Str "det er" &&& (||$* [| "beklageligt"; "bevist"; "forståeligt"; 
                                          "klart"; "indiskutabelt"; 
                                          "nødvendigt"; "oplagt"; "påfaldende"; "velkendt" |]),
        (||$* [| "anerkendte"; "enkelte"; "de fleste"; "danske"; "isolerede"; 
                 "omhyggelige";  
                 "samtlige"; "troværdige"; "uafhængige"; 
                 "udenlandske"; "visse"; "vores" |])
              &&& (||$* [| "analyser"; "forskere"; "iagttagere"; "resultater";
                         "studier"; "undersøgelser" |])
              &&& (||$* [| "antyder"; "demonstrerer"; "fastslår"; 
                         "lader formode"; "påpeger"; "viser" |])
              ]))
    &&& fun () -> Str "at" 
    &&& ledsaetning
    ||| hovedsaetning)
    &&& (||* [|
        fun () -> Str "," &&& konjunktion &&& ledsaetning
                &&& (fun () -> if ssh 0.05 then Str (", og " + konjunktion()) &&& ledsaetning else Tom),
        fun () -> Str "," &&& ((Str "der") ||| (Str "som")) &&& (if ssh 0.5 then ||$* [| "af denne årsag"; "derfor"; "følgelig"; 
                                       "samtidig"; "sideløbende"; 
                                       "således"; "trods dette" |] &&& adverbial &&& verbPraesIndAkt &&& nominal else Tom)
        ,fun () -> if ssh 0.3 then Str "." &&& Str "For det første fordi" &&& ledsaetning &&& Str "," &&& Str "og for det andet fordi" &&& ledsaetning else Tom
        ]))

let konstatering = fun () -> Format.begyndelse (konstatering ()) && Str "."

let raesonnement =
    konjunktion
    &&& ledsaetning
    &&& Str ","
    &&& ((||$* [| "bør"; "kan"; "må"; "skal" |]) &&& Str "det"
         &&& (||$* [| "antages"; "betones"; "betvivles"; "forudsættes";
                     "konstateres"; "pointeres"; 
                     "påpeges"; "understreges" |])
         ||| (||$* [| "bør"; "kan"; "må"; "skal" |]) 
             &&& (||$* [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |]) 
             &&& (||$* [| "acceptere"; "anerkende"; "antage"; "beklage"; 
                        "sikre"; "forudsætte"; "konstatere" |]))
    &&& Str "at" 
    &&& (ledsaetning
         ||| Str "dette" &&& adverbial &&& verbPraesIndAkt &&& nominal)

let raesonnement = fun () -> Format.begyndelse (raesonnement ()) && Str "."

let konsekvens =
    (||$* [|Str "det" 
            &&& (||$* [| "følger"; "indses"; "konkluderes"; "ses" |]),
          (||$* [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |])
             &&& (||$* [| "konkluderer"; "ser"; "slutter" |]),
          Str "der" &&& Str "gælder"
         ])
    &&& (||$* [| "altså"; "da"; "derfor"; "endda"; "endvidere"; 
                 "nu"; "straks"; "således"; "tillige";
                 "uden videre"; "ret umiddelbart"; "umiddelbart" |])
    &&& Str "," &&& Str "at"
    &&& ledsaetning
    &&& (fun () -> if ssh 0.7 then Str "," &&& Str "og" &&& Str "at" &&& ledsaetning else Tom)
    ||| (||$* [| "altså"; "af disse grunde"; "derfor"; "klart nok";
               "følgelig"; "således" |])
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
    &&& Str "Centret vil være en oplagt partner for det nyligt" 
    &&& Str "foreslåede center for" &&& centernavn () &&& Str "," 
    &&& Str "ligesom der bør kunne opnås en frugtbar symbiose med"
    &&& Str "centret for" &&& centernavn () &&& Str "."

let oprettelse center =
    fun () -> Format.begyndelse (
        ( ||$* [|"de anførte"; "ovenstående"; "de opregnede"]
          &&& ||$* [|"argumenter"; "betragtninger"; "forhold"; "grunde";
                    "konstateringer"; "overvejelser"; 
                    "ræsonnementer"]
          &&& ||$* [|"fører"; "leder"]
          &&& (fun () -> if ssh 0.5 then ||$* [|"logisk"; "nødvendigvis"; "os"; "uomgængeligt"] else Tom)
          &&& (fun () -> if ssh 0.3 then Str "frem" else Tom)
          &&& Str "til den konklusion at der" 
          &&& ||$* [|"er behov for"; "må oprettes"; "bør etableres"]
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
