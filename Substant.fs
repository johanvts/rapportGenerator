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
 RegS (Fk,  t, "indsats", "-en", "-er", "-ne", [|""; "forsknings"|]);
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
 RegS (Itk, t, "råd", "-et", "-", "-ene", 
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
 RegS (Fk,  t, "undersøgelse", "-n", "-r", "-ne", [|""|]);
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

let haddock = [|
 RegS (Fk, f, "abemås", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "agurketud", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "analfabet", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "asfaltcowboy", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "bacille", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "bandit", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "barnerøver", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "basilisk", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "bedrager", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "bavian", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "bisse", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "bladanblander", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "brandstifter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "bums", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "burgøjser", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "bæst", "-et", "-", "-ene", [|""|]);
RegS (Fk, f, "bøddel", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "bøhtosse", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "bølle", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "børste", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "cykeltyv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "daddelplukker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "desertør", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "dovendyr", "-et", "-", "-ene", [|""|]);
RegS (Fk, f, "drukkenbolt", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "egoist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "fascist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "fedtblære", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "fedtemikkel", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "fedthalefår", "-et", "-", "-ene", [|""|]);
RegS (Fk, f, "fladpande", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "fjollerik", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "forbryder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "forlismand", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "fortidsuhyre", "-t", "-r", "-ne", [|""|]);
RegS (Fk, f, "frysefrederik", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "fyldebøtte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "gangster", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "gespenst", "-et", "-", "-ene", [|""|]);
RegS (Fk, f, "grimrian", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "grobrian", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "haleneger", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "hallunk", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "hulepindsvin", "-et", "-", "-ene", [|""|]);
RegS (Fk, f, "hærværksmand", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "høvl", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "igle", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "ignorant", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "individ", "-et", "-", "-ene", [|""|]);
RegS (Fk, f, "jubeltorsk", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "justitsmorder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "kakerlak", "-ken", "-ker", "-kene", [|""|]);
RegS (Fk, f, "kannibal", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "karnevalssørøver", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "klaptorsk", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "kleptoman", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "klodrian", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "klodsmajor", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "knoldvækst", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "kryb", "-et", "", "-ene", [|""|]);
RegS (Fk, f, "krybskytte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "krudtugle", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "kvajpande", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "kvælstofbacille", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "kæltring", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "køter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "laban", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "lakaj", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "landevejsrøver", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "landkrabbe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "landsforræder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "lommetyv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "lejesvend", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "lemmedasker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "lumskebuks", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "lurendrejer", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "lusepuster", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "luskebuks", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "lydpottelus", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "lystmorder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "læderjakke", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "løjser", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "makrelslugere", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "marxist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "massemorder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "menneskefjende", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "menneskeæder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "misdæder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "mordbrænder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "morder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "motorbølle", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "møgdyr", "-et", "-", "-ene", [|""|]);
RegS (Fk, f, "pirat", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "plebejer", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "prøjser", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "pladderabe", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "pungdyr", "-et", "", "-ene", [|""|]);
RegS (Fk, f, "pyroman", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "racist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "rottefjæs", "-et", "-", "-ene", [|""|]);
RegS (Fk, f, "sadist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "samfundssnylter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "sandalslæber", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "sandloppe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "sandmide", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "sinke", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "sjofelist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "sjover", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "sjuft", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "skadedyr", "-et", "", "-ene", [|""|]);
RegS (Fk, f, "skallesmækker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "skamstøtte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "skunk", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "skurk", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "skvatdragon", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "skvatmelon", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "skægabe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "slagsbror", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "slambert", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "slapsvans", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "sleske spytslikker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "slubbert", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "slyngel", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "smugler", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "smørtyv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "snigmorder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "snigskytte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "snoabe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "snog", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "snydetamp", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "snylter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "sortbørsgrosserer", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "spritbilist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "spritter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "spruttyv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "sprællemand", "-en", "-mænd", "-mændene", [|""|]);
RegS (Fk, f, "spyflue", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "starut", "-en", "-ter", "-terne", [|""|]);
RegS (Fk, f, "stikker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "stymper", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "subjekt", "-et", "", "-ene", [|""|]);
RegS (Fk, f, "superskurk", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "sut", "-ten", "-ter", "-terne", [|""|]);
RegS (Fk, f, "svamp", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "svin", "-et", "", "-ene", [|""|]);
RegS (Fk, f, "svindler", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "svirebror", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "svumpukkel", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "søndagsrytter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "søpindsvinefjæs", "-et", "", "-ene", [|""|]);
RegS (Fk, f, "sørøver", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "tale-delirist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "tamp", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "tangloppe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "terrorist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "torskepande", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "trafikbisse", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "trompetsnegl", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "tyran", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "tyveknægt", "-en", "-e", "-ene", [|""|]);
RegS (Fk, f, "tøffelhelt", "-en", "-e", "-ene", [|""|]);
RegS (Fk, f, "tøsedreng", "-en", "-e", "-ene", [|""|]);
RegS (Fk, f, "udbytter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "undermåler", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "vagabond", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "vampyr", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "vandal", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "vandrotte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "varulv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, f, "varyl", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "vatnisse", "-n", "-r", "-ne", [|""|]);
RegS (Fk, f, "voldsmand", "-en", "-mænd", "-mændene", [|""|]);
RegS (Fk, f, "æselmassør", "-en", "-er", "-ne", [|""|]);
RegS (Fk, f, "øgle", "-n", "-r", "-ne", [|""|]);
|]


let boejS (RegS(koen, artk, ubsen, besen, ubsfl, besfl, _)) form =
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
    let subst = vaelgfra (Array.concat [substantiver;haddock])
    let (former, praefikser) = muligS subst
    let praef                = vaelgfra praefikser 
    let form = match formkrav with
                    | None -> vaelgfra former
                    | Some (b, Flt) -> if taelleligS subst then (b, Flt) else (b, Ent)
                    | Some form     -> form
    try
        let nom = dobbeltS praefikser praef &&& Str (praef + boejS subst form) 
        (form, koenS subst, nom, taelleligS subst)
    with
        | Umulig _ -> vaelg formkrav

let rec possesiv = function
  | Tom        -> Tom
  | Str s    -> Str (s + "s")
  | OrdSeq (s1, s2) ->
      match possesiv s2 with
      | Tom -> possesiv s1
      | s2' -> s1 &&& s2'
