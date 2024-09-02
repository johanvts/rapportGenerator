module Substant

open Basis

type substantiv =
    RegS of genus * bool * string * string * string * string * (string array)
         (* artk    bes?   ubs/en   bes/en   ubs/fl   bes/fl   præfixer *)    

let f = false
let t = true

let substantiver = [|
 RegS (Fk,  t, "aktivitet", "-en", "-er", "-ne", [|""|]);
 RegS (Fk,  t, "analyse", "-n", "-r", "-ne", 
       [|""; "data"; "problem"; "samfunds"; "system"|]);
 RegS (Fk,  f, "anvendelse", "-n", "-r", "-ne", 
       [|""; "software"; "teknologi"|]);
 RegS (Itk, f, "arbejde", "-t", "", "", 
       [|""; "forsknings"; "udviklings"; "sam"; "udrednings"|]);
 RegS (Itk, t, "behov", "-et", "-", "-ene", 
       [|""; "organisations"|]);
 RegS (Itk, t, "center", "centret", "centre", "-ne", [|""|]);
 RegS (Itk, f, "design", "-et", "", "", [|""|]);
 RegS (Fk,  t, "dimension", "-en", "-er", "-ne", [|""|]);
 RegS (Fk,  f, "effektivitet", "-en", "", "", [|""|]);
 RegS (Itk, t, "eksempel", "eksemplet", "eksempler", "-ne", [|""|]);
 RegS (Fk,  t, "ekspertise", "-n", "-r", "-ne", 
       [|""; "organisations"; "software"; "uddannelses"|]);
 RegS (Fk,  t, "enhed", "-en", "-er", "-ne", 
       [|"";  "center"; "forsknings"|]);
 RegS (Fk,  t, "evaluering", "-en", "-er", "-ne", 
       [|""; "projekt"; "forsknings"|]);
 RegS (Fk,  t, "faktor", "-en", "-er", "-ne", [|""|]);
 RegS (Fk,  f, "forandring", "-en", "-er", "-ne", [|""|]);
 RegS (Itk, t, "foregangsland", "-et", "-e", "-ne", [|""|]);
 RegS (Fk,  t, "forsker", "-en", "-e", "forskerne", 
       [|""; "erhvervs"; "kontrakt"; "senior"|]);
 RegS (Fk,  f, "forskning", "-en", "", "", 
       [|""; "grund"; "kontrakt"|]);
 RegS (Itk, t, "forslag", "-et", "-", "-ene", [|""|]);
 RegS (Fk,  t, "gruppe", "-n", "-r", "-ne", 
       [|""; "arbejds"; "forsker"|]);
 RegS (Itk, f, "indhold", "-et", "", "", [|""|]);
 RegS (Fk,  f, "indførelse", "-n", "", "", 
       [|"edb-"; "system"; "teknologi"|]);
 RegS (Fk,  t, "indsats", "-en", "", "", [|""; "forsknings"|]);
 RegS (Itk, t, "initiativ", "-et", "-er", "-ne", 
       [|""; "forsknings"; "omstillings"|]);
 RegS (Fk,  t, "institution", "-en", "-er", "-ne", 
       [|""; "forsknings"|]);
 RegS (Itk, t, "kommissorium", "kommissoriet", "kommissorier", "-ne", [|""|]);
 RegS (Fk,  f, "kommunikation", "", "", "", [|""|]);
 RegS (Itk, t, "koncept", "-et", "-er", "-ne", 
       [|""; "kvalitets"|]);
 RegS (Fk,  t, "konklusion", "-en", "-er", "-ne", [|""|]);
 RegS (Fk,  f, "koordinering", "-en", "", "", [|""|]);
 RegS (Fk,  t, "kultur", "-en", "", "", [|""|]);
 RegS (Fk,  t, "kvalitet", "-en", "-er", "-ne", 
       [|""; "livs"; "software"|]);
 RegS (Fk,  f, "læring", "-en", "", "", [|""|]);
 RegS (Fk,  t, "metode", "-n", "-r", "-ne", [|""|]);
 RegS (Itk, t, "miljø", "-et", "-er", "-ne", 
       [|""; "forsknings"; "lærings"; "undervisnings"|]);
 RegS (Itk, t, "ministerium", "ministeriet", "ministerier", "-ne", 
       [|""; "forsknings"; "miljø"; "undervisnings"|]);
 RegS (Fk,  t, "model", "-len", "-ler", "-ne", [|""|]);
 RegS (Itk, t, "netværk", "-et", "-", "-erne", [|""|]);
 RegS (Itk, t, "niveau", "-et", "-er", "-ne", [|""|]);
 RegS (Itk, t, "område", "-t", "-r", "-ne", 
       [|""; "indsats"; "problem"|]);
 RegS (Fk,  f, "omstilling", "-en", "", "", [|""|]);
 RegS (Fk,  t, "organisation", "-en", "-er", "-ne", 
       [|""; "arbejds"|]);
 RegS (Itk, t, "paradigme", "-t", "-r", "-ne",
       [|""; "omstillings"; "videns"|]);
 RegS (Fk,  t, "parameter", "-en", "parametre", "-ne", 
       [|""; "evaluerings"; "konkurrence"|]);
 RegS (Itk, t, "partnerskab", "-et", "-er", "-ne", [|""|]);
 RegS (Fk,  t, "politik", "-ken", "", "", 
       [|""; "forsknings"; "IT-"; "teknologi"; "uddannelses"|]);
 RegS (Itk, t, "potentiale", "-t", "-r", "-ne", 
       [|""; "samarbejds"; "udviklings"|]);
 RegS (Itk, t, "problem", "-et", "-er", "-ne", [|""; "samarbejds"|]);
 RegS (Fk,  t, "proces", "-sen", "-ser", "-ne", 
       [|""; "beslutnings"; "forsknings"; "uddannelses"; "udviklings"|]);
 RegS (Itk, t, "program", "-met", "-mer", "-ne", 
       [|"forsknings"; "udviklings"; "undervisnings"|]);
 RegS (Itk, t, "projekt", "-et", "-er", "-ne", 
       [|""; "forsknings"; "udviklings"|]);
 RegS (Fk,  t, "prototype", "-n", "-r", "-ne", [|""; "software"|]);
 RegS (Fk,  t, "rapport", "-en", "-er", "-ne", 
       [|""; "arbejds"; "forsknings"|]);
 RegS (Fk,  t, "relation", "-en", "-er", "-ne", [|""|]);
 RegS (Itk, t, "resultat", "-et", "-er", "-ne", 
       [|""; "evaluerings"; "forsknings"|]);
 RegS (Fk,  t, "ressource", "-n", "-r", "-ne", [|""|]);
 RegS (Itk, t, "r�d", "-et", "-", "-ene", 
       [|"center"; "forsknings"; "uddannelses"|]);
 RegS (Itk, t, "samfund", "-et", "-", "-ene", 
       [|""; "industri"; "info-"; "informations"; "videns"|]);
 RegS (Itk, f, "samspil", "-let", "", "", [|""|]);
 RegS (Itk, t, "scenario", "scenariet", "scenarier", "-ne", [|""|]);
 RegS (Fk,  t, "situation", "-en", "-er", "-ne", 
       [|""; "arbejds"; "brugs"|]);
 RegS (Fk,  f, "software", "-n", "", "", 
       [|""; "apparat"|]);
 RegS (Fk,  t, "stilling", "-en", "-er", "-ne", 
       [|"ind"; "problem"|]);
 RegS (Fk,  t, "struktur", "-en", "-er", "-ne", 
       [|""; "virksomheds"; "samfunds"|]);
 RegS (Fk,  t, "synsvinkel", "synsvinklen", "synsvinkler", "-ne",
       [|"bruger"; "leder"; "organisations"; "software"; "teknologi"|]);
 RegS (Itk, t, "system", "-et", "-er", "-ne", 
       [|""; "edb-"; "informations"; "IT-"; "kommunikations"|]);
 RegS (Fk,  f, "teknik", "-ken", "-ker", "-ne", 
       [|""; "edb-"; "produktions"|]);
 RegS (Fk,  t, "teknologi", "-en", "-er", "-ne", 
       [|""; "informations"; "kommunikations"; "koncern"; 
         "multimedie"; "netværks"; "software"|]);
 RegS (Fk,  f, "teori", "-en", "-er", "-ne", [|""|]);
 RegS (Fk,  t, "tilpasning", "-en", "-er", "-ne", [|""|]);
 RegS (Fk,  f, "uddannelse", "-n", "-r", "-ne", 
       [|"efter"; "videre"|]);
 RegS (Fk,  t, "unders�gelse", "-n", "-r", "-ne", [|""|]);
 RegS (Itk, t, "universitet", "-et", "-er", "-ne", [|""|]);
 RegS (Itk, t, "udvalg", "-et", "-", "-ene", [|""|]);
 RegS (Itk, f, "udstyr", "-et", "", "", [|""|]);
 RegS (Fk,  t, "udvikling", "-en", "", "",
       [|""; "IT-"; "samfunds"; "edb-"; "kvalitets"; "produkt"; "videre"|]);
 RegS (Fk,  f, "viden", "-en", "", "", [|""|]);
 RegS (Fk,  t, "virksomhed", "-en", "-er", "-ne", 
       [|""; "software"|]);
 RegS (Fk,  t, "virkning", "-en", "-er", "-ne", [|""|])
|]

let boejS (RegS(koen, artk, ubsen, besen, ubsfl, besfl, _)) form =
    printfn "%A %A -%A" ubsen besen form
    match form with
      | (Ubs, Ent) -> ubsen
      | (Bes, Ent) -> tilfoej ubsen besen
      | (Ubs, Flt) -> tilfoej ubsen ubsfl
      | (Bes, Flt) -> tilfoej (tilfoej ubsen ubsfl) besfl

let koenS (RegS(koen, _, _, _, _, _, _)) = koen

let taelleligS (RegS(_, artk, _, _, _, _, _)) = artk

let muligS (RegS(koen, artk, ubsen, besen, ubsfl, besfl, prefs)) =
    ((Array.choose (fun (s, f) -> if s="" then None else Some f)
                      [|(ubsen, (Ubs, Ent)); 
                       (besen, (Bes, Ent)); 
                       (ubsfl, (Ubs, Flt)); 
                       (besfl, (Bes, Flt))|]),
    prefs)

let dobbeltS praefikser pref = 
    if Array.length praefikser > 2 && pref <> "" && ssh 0.2 then
        let pref1 = vaelgikke [pref; ""] praefikser
        if pref1.Substring(pref1.Length - 1, 1) = "-" then Str pref1 else Str (pref1 + "-")
        &&& (if ssh 0.3 then (if ssh 0.2 then Str "og/eller" else Str "eller") else Str "og")
    else
        Tom

let rec vaelg formkrav =
    let subst = vaelgfra substantiver
    let (former, praefikser) = muligS subst
    let praef                = vaelgfra praefikser 
    let form = match formkrav with
                    | None -> vaelgfra former
                    | Some (b, Flt) -> if taelleligS subst then (b, Flt) else (b, Ent)
                    | Some form     -> form
    let nom = dobbeltS praefikser praef &&& Str (praef + boejS subst form) 
    try
        (form, koenS subst, nom, taelleligS subst)
    with
        | Umulig _ -> vaelg formkrav

let rec possesiv = function
  | Tom        -> Tom
  | Str s    -> Str (s + "s")
  | OrdSeq (s1, s2) ->
      match s2 with
      | Tom -> possesiv s1
      | s2' -> s1 &&& s2'
