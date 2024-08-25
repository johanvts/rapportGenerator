module Led

open Basis

let adj form koen =
    let adj = Adj.vaelg ()
    Str (Adj.boej adj form koen)
    &&& (if ssh 0.15 then
             Str "og" &&& Str (Adj.boej (Adj.vaelgikke [adj]) form koen)
         else
             Tom)

let advadj form koen =
    (if ssh 0.2 then Smaaord.vaelgadv () else Tom) &&& adj form koen

let nominal formkrav =
    let (form as (_, tal), koen, nom, taellelig) = 
        Substant.vaelg formkrav

    let saadan () = 
        if ssh 0.03 then
            Str "som sådan"
        else
            Tom

    let possesiv () = 
        Substant.possesiv (Substant.vaelg (Some (Bes, Ent))).Item3

    let nled = 
        match form with
        | (Ubs, Flt) ->
            if ssh 0.5 then
                Smaaord.artikel ((Bes, Flt), koen)
                &&& advadj (Bes, Flt) koen 
                &&& nom 
                &&& saadan ()
            else
                (if ssh 0.4 then
                    possesiv ()
                 else if ssh 0.6 then 
                    Str (vaelgfra [| "visse"; "nogle"; "alle"; "samtlige"; "forskellige"; "andre"; "enkelte" |])
                 else 
                    Tom)
                &&& advadj form koen
                &&& nom
        | (Ubs, Ent) ->
            if taellelig then 
                if ssh 0.4 then
                    Smaaord.artikel ((Bes, Ent), koen)
                    &&& advadj (Bes, Ent) koen
                    &&& nom 
                    &&& saadan ()
                else if ssh 0.4 then
                    Smaaord.artikel (form, koen) 
                    &&& advadj form koen
                    &&& nom
                else
                    possesiv ()
                    &&& advadj (Bes, Ent) koen
                    &&& nom
            else 
                advadj form koen &&& nom
        | (Bes, _) ->
            nom &&& saadan ()
    (tal, koen, nled)
