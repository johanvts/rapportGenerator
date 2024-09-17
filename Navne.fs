module Navne

open Basis

let praefikser = [| "Brønd"; "Bøg"; "Dal"; "Dals"; "Dam"; "Dyb"; "Hede"; "Høj";
                 "Holm"; "Kål"; "Lang"; "Linde"; "Magle";
                 "Malm"; "Mel"; "Møl"; "Nør"; "Ring"; "Rosen";
                  "Skov"; "Strand"; "Stub"; "Sund"; "Sø"; "Sønder"; "Tud"|]

let suffikser =
    [|"ager"; "balle"; "bakke"; "borg"; "by"; "gaard"; "gård"; "holm"; "kjær"; "kær"; 
      "lund"; "sted"; "toft"; "vad"|]

let senner = 
    [|"Ander"; "Andreas"; "Bertel"; "Clau"; "Christian"; "Erik"; "Eskild"; "Han"; "Jen"; "Johan"; 
      "Jørgen"; "Karl"; "Knud"; "Konrad"; "Kristoffer"; "Lar";"Iver";
      "Lorentz"; "Morten"; "Niel"; "Ole"; "Peder"; "Peter"; "Poul"; "Sivert"; "Søren"; "Thom"|]

let enere =
    [| "Møller"; "Dahl"; "Bang"; "Christiani"; "Frost"; "Enevoldsen"; "Fischer"; "Fris"; "Bonde"; "Juul"; "Hoffmann" ; "Holst"; "Buch"; "Bruun"; "Berg"; "Winther"; "Ravn"; "Ahmad"|]

let udenlandske =
    [| "Schimdt"; "Williams"; "Taylor"; "Brown"; "Watson"; "Lewis"; "Biermann" ; "Dusefante" ; "Ackermann" ;"Adenauer"; "Bauer"; "Rossi"; "Bianchi"; "Bertrand"; "Becker"; "Russo" ; "Buhlendorf"|]

let forbogstav antal =
    let mulige = "ABCDEFGHIJKLMNOPRST"
    let vaelg () =
       [| mulige.[terning (mulige.Length - 1)]; '.'|]
    Array.init antal (fun _ -> vaelg ()) |> Array.concat |>  System.String

let navn () =
    Str (forbogstav (terning 2))
    &&& Str (vaelgfra praefikser + vaelgfra suffikser +
             if ssh 0.4 then
                 (if ssh 0.3 then "-" else " ")
                 + vaelgfra senner + "sen"
             else ""
             )

let titel ()  =
    let praefikser =
      [|"afdelings"; "center"; "divisions"; "forsknings"; "informatik";
      "kvalitets"; "sektions"; "sektor"; "senior"; "uddannelses"; 
      "udviklings"; "vice"|]
    let suffikser =
     [|"chef"; "direktør"; "konsulent"; "koordinator"; 
      "leder"; "rådgiver"; "sekretær"; "specialist"|]
    Str (vaelgfra praefikser + vaelgfra suffikser)

let person () = titel () &&& navn ()
