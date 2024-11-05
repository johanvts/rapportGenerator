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
 RegS (Fk, t, "abemås", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "agurketud", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "analfabet", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "asfaltcowboy", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "bacille", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "bandit", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "barnerøver", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "basilisk", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "bedrager", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "bavian", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "bisse", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "bladanblander", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "brandstifter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "bums", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "burgøjser", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "bæst", "-et", "-", "-ene", [|""|]);
RegS (Fk, t, "bøddel", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "bøhtosse", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "bølle", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "børste", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "cykeltyv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "daddelplukker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "desertør", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "dovendyr", "-et", "-", "-ene", [|""|]);
RegS (Fk, t, "drukkenbolt", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "egoist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "fascist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "fedtblære", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "fedtemikkel", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "fedthalefår", "-et", "-", "-ene", [|""|]);
RegS (Fk, t, "fladpande", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "fjollerik", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "forbryder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "forlismand", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "fortidsuhyre", "-t", "-r", "-ne", [|""|]);
RegS (Fk, t, "frysefrederik", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "fyldebøtte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "gangster", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "gespenst", "-et", "-", "-ene", [|""|]);
RegS (Fk, t, "grimrian", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "grobrian", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "haleneger", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "hallunk", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "hulepindsvin", "-et", "-", "-ene", [|""|]);
RegS (Fk, t, "hærværksmand", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "høvl", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "igle", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "ignorant", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "individ", "-et", "-", "-ene", [|""|]);
RegS (Fk, t, "jubeltorsk", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "justitsmorder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "kakerlak", "-ken", "-ker", "-kene", [|""|]);
RegS (Fk, t, "kannibal", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "karnevalssørøver", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "klaptorsk", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "kleptoman", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "klodrian", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "klodsmajor", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "knoldvækst", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "kryb", "-et", "", "-ene", [|""|]);
RegS (Fk, t, "krybskytte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "krudtugle", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "kvajpande", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "kvælstofbacille", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "kæltring", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "køter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "laban", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "lakaj", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "landevejsrøver", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "landkrabbe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "landsforræder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "lommetyv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "lejesvend", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "lemmedasker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "lumskebuks", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "lurendrejer", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "lusepuster", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "luskebuks", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "lydpottelus", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "lystmorder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "læderjakke", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "løjser", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "makrelslugere", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "marxist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "massemorder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "menneskefjende", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "menneskeæder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "misdæder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "mordbrænder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "morder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "motorbølle", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "møgdyr", "-et", "-", "-ene", [|""|]);
RegS (Fk, t, "pirat", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "plebejer", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "prøjser", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "pladderabe", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "pungdyr", "-et", "", "-ene", [|""|]);
RegS (Fk, t, "pyroman", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "racist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "rottefjæs", "-et", "-", "-ene", [|""|]);
RegS (Fk, t, "sadist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "samfundssnylter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "sandalslæber", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "sandloppe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "sandmide", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "sinke", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "sjofelist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "sjover", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "sjuft", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "skadedyr", "-et", "", "-ene", [|""|]);
RegS (Fk, t, "skallesmækker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "skamstøtte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "skunk", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "skurk", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "skvatdragon", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "skvatmelon", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "skægabe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "slagsbror", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "slambert", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "slapsvans", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "sleske spytslikker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "slubbert", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "slyngel", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "smugler", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "smørtyv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "snigmorder", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "snigskytte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "snoabe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "snog", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "snydetamp", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "snylter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "sortbørsgrosserer", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "spritbilist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "spritter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "spruttyv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "sprællemand", "-en", "-mænd", "-mændene", [|""|]);
RegS (Fk, t, "spyflue", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "starut", "-en", "-ter", "-terne", [|""|]);
RegS (Fk, t, "stikker", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "stymper", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "subjekt", "-et", "", "-ene", [|""|]);
RegS (Fk, t, "superskurk", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "sut", "-ten", "-ter", "-terne", [|""|]);
RegS (Fk, t, "svamp", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "svin", "-et", "", "-ene", [|""|]);
RegS (Fk, t, "svindler", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "svirebror", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "svumpukkel", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "søndagsrytter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "søpindsvinefjæs", "-et", "", "-ene", [|""|]);
RegS (Fk, t, "sørøver", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "tale-delirist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "tamp", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "tangloppe", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "terrorist", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "torskepande", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "trafikbisse", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "trompetsnegl", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "tyran", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "tyveknægt", "-en", "-e", "-ene", [|""|]);
RegS (Fk, t, "tøffelhelt", "-en", "-e", "-ene", [|""|]);
RegS (Fk, t, "tøsedreng", "-en", "-e", "-ene", [|""|]);
RegS (Fk, t, "udbytter", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "undermåler", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "vagabond", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "vampyr", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "vandal", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "vandrotte", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "varulv", "-en", "-e", "-ne", [|""|]);
RegS (Fk, t, "varyl", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "vatnisse", "-n", "-r", "-ne", [|""|]);
RegS (Fk, t, "voldsmand", "-en", "-mænd", "-ene", [|""|]);
RegS (Fk, t, "æselmassør", "-en", "-er", "-ne", [|""|]);
RegS (Fk, t, "øgle", "-n", "-r", "-ne", [|""|]);
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
