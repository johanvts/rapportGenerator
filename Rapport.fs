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
        printfn "%s -> %i" mulig klasse
        printfn "%i" valgNummer
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
              "f�rst og fremmest"; "generelt";
              "gradvis"; "ikke"; "ikke n�dvendigvis"; 
              "indadtil"; "indirekte"; "i det lange l�b"; 
              "i ringe grad";
              "isoleret set";
              "kun"; "kun sj�ldent"; "langt oftere"; "l�st sagt";
              "med tiden"; "midlertidigt"; "muligvis";
              "måske"; "måske ikke"; "noget indirekte"; "nok ikke"; 
              "ofte"; "partielt"; "potentielt"; 
              "ret typisk"; "ret utvetydigt";  
              "sj�ldent"; "så at sige"; 
              "temmelig entydigt"; "tendentielt"; 
              "utvivlsomt"; "kun vanskeligt"; "v�sentligst"|]
              
let verbPraesIndAkt = 
    vaelgNyUg [|"accentuerer"; "afmystificerer"; "angår"; "belyser"; "begrunder"; 
           "ber�rer"; "beskriver"; 
           "besv�rligg�r"; "effektiviserer"; "eksternaliserer"; 
           "erstatter";
           "forandrer"; "foregriber"; "fornyer";
           "forst�rker"; "fortr�nger"; "fremmer"; "h�mmer";
           "implicerer"; "involverer"; "karakteriserer"; 
           "kendetegner"; "klarl�gger";
           "komplicerer";
           "modarbejder"; "moderniserer"; "modsvarer"; 
           "omdefinerer";
           "problematiserer"; "profilerer"; "p�virker"; 
           "reducerer";
           "styrker"; "st�tter"; "sv�kker"; "udnytter"; "udvikler";
           "underst�tter"; "vedr�rer"|]
           
let konjunktion =
    vaelgNyUg [|"da"; "da"; "eftersom"; "eftersom"; "fordi"; "forudsat"; 
           "hvis"; "ikke mindst fordi"; "mens"; "netop fordi";
           "n�r"; "n�r blot"; "p� trods af at"; 
           "selvom"; "selvom"; "sk�nt"; "s�fremt"|]
           
let ledsaetning =
    nominal @@@ adverbial @@@ verbPraesIndAkt @@@ nominal

let ledsaetning2 =
    verbPraesIndAkt @@@ nominal @@@ adverbial @@@ nominal

let hovedsaetning =
    nominal @@@ verbPraesIndAkt @@@ adverbial @@@ nominal

let konstatering = 
    (vaelgLige [| s"det er" @@@ (vaelgNyUg [| "beklageligt"; "bevist"; "forst�eligt"; 
                                          "klart"; "indiskutabelt"; 
                                          "n�dvendigt"; "oplagt"; "p�faldende"; "velkendt" |]);
                vaelgNyUg [| "anerkendte"; "enkelte"; "de fleste"; "danske"; "isolerede"; 
                           "omhyggelige";  
                           "samtlige"; "trov�rdige"; "uafh�ngige"; 
                           "udenlandske"; "visse"; "vores" |]
              @@@ vaelgNyUg [| "analyser"; "forskere"; "iagttagere"; "resultater";
                             "studier"; "unders�gelser" |]
              @@@ vaelgNyUg [| "antyder"; "demonstrerer"; "fastsl�r"; 
                             "lader formode"; "p�peger"; "viser" |]
              |]
     @@@ s"at" 
     @@@ ledsaetning
     ||| hovedsaetning)
     @@@ vaelgLige [| s"," @@@ konjunktion @@@ ledsaetning
                           @@@ (0.05 >>>
                                (s"," @@@ s"og " @@@ konjunktion @@@ ledsaetning));
                      s","
                           @@@ (s"der" ||| s"som")
                           @@@ (0.5 >>> vaelgNyUg [| "af denne �rsag"; "derfor"; "f�lgelig"; 
                                       "samtidig"; "sidel�bende"; 
                                       "s�ledes"; "trods dette" |])
                           @@@ adverbial @@@ verbPraesIndAkt @@@ nominal;
                    0.3 >>>(s"."
                            @@@ s"For det f�rste fordi" @@@ ledsaetning
                            @@@ s","
                            @@@ s"og for det andet fordi" @@@ ledsaetning)
                 |]
(*
let konstatering = fun () -> Format.begyndelse (konstatering ()) && Str "."

let raesonnement =
    konjunktion
    &&& ledsaetning
    &&& Str ","
    &&& ((vaelgNyUg [| "b�r"; "kan"; "m�"; "skal" |]) &&& Str "det"
         &&& (vaelgNyUg [| "antages"; "betones"; "betvivles"; "foruds�ttes";
                     "konstateres"; "pointeres"; 
                     "p�peges"; "understreges" |])
         ||| (vaelgNyUg [| "b�r"; "kan"; "m�"; "skal" |]) 
             &&& (vaelgNyUg [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |]) 
             &&& (vaelgNyUg [| "acceptere"; "anerkende"; "antage"; "beklage"; 
                        "sikre"; "foruds�tte"; "konstatere" |]))
    &&& Str "at" 
    &&& (ledsaetning
         ||| Str "dette" &&& adverbial &&& verbPraesIndAkt &&& nominal)

let raesonnement = fun () -> Format.begyndelse (raesonnement ()) && Str "."

let konsekvens =
    (vaelgNyUg [|Str "det" 
            &&& (vaelgNyUg [| "f�lger"; "indses"; "konkluderes"; "ses" |]),
          (vaelgNyUg [| "arbejdsgruppen"; "man"; "udvalget"; "vi" |])
             &&& (vaelgNyUg [| "konkluderer"; "ser"; "slutter" |]),
          Str "der" &&& Str "g�lder"
         ])
    &&& (vaelgNyUg [| "alts�"; "da"; "derfor"; "endda"; "endvidere"; 
                 "nu"; "straks"; "s�ledes"; "tillige";
                 "uden videre"; "ret umiddelbart"; "umiddelbart" |])
    &&& Str "," &&& Str "at"
    &&& ledsaetning
    &&& (fun () -> if ssh 0.7 then Str "," &&& Str "og" &&& Str "at" &&& ledsaetning else Tom)
    ||| (vaelgNyUg [| "alts�"; "af disse grunde"; "derfor"; "klart nok";
               "f�lgelig"; "s�ledes" |])
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
    && mksek (3 + terning 5) (vaelgLige [| konstatering; raesonnement; konsekvens |])
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
    &&& Str "Centret vil v�re en oplagt partner for det nyligt" 
    &&& Str "foresl�ede center for" &&& centernavn () &&& Str "," 
    &&& Str "ligesom der b�r kunne opn�s en frugtbar symbiose med"
    &&& Str "centret for" &&& centernavn () &&& Str "."

let oprettelse center =
    fun () -> Format.begyndelse (
        ( vaelgNyUg [|"de anf�rte"; "ovenst�ende"; "de opregnede"]
          &&& vaelgNyUg [|"argumenter"; "betragtninger"; "forhold"; "grunde";
                    "konstateringer"; "overvejelser"; 
                    "r�sonnementer"]
          &&& vaelgNyUg [|"f�rer"; "leder"]
          &&& (fun () -> if ssh 0.5 then vaelgNyUg [|"logisk"; "n�dvendigvis"; "os"; "uomg�ngeligt"] else Tom)
          &&& (fun () -> if ssh 0.3 then Str "frem" else Tom)
          &&& Str "til den konklusion at der" 
          &&& vaelgNyUg [|"er behov for"; "m� oprettes"; "b�r etableres"]
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
