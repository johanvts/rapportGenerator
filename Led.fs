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
        Substant.possesiv (Substant.vaelg (Some (bes, ent)).Item3)

    let nled = 
        match form with
        | (ubs, flt) ->
            if ssh 0.5 then
                Smaaord.artikel ((bes, flt), koen)
                && advadj (bes, flt) koen 
                && nom 
                && saadan ()
            else
                (if ssh 0.4 then
                    possesiv ()
                 else if ssh 0.6 then 
                    Str (vaelgfra [| "visse"; "nogle"; "alle"; "samtlige"; "forskellige"; "andre"; "enkelte" |])
                 else 
                    Tom)
                && advadj form koen
                && nom
        | (ubs, ent) ->
            if taellelig then 
                if ssh 0.4 then
                    Smaaord.artikel ((bes, ent), koen)
                    && advadj (bes, ent) koen
                    && nom 
                    && saadan ()
                else if ssh 0.4 then
                    Smaaord.artikel (form, koen) 
                    && advadj form koen
                    && nom
                else
                    possesiv ()
                    && advadj (bes, ent) koen
                    && nom
            else 
                advadj form koen && nom
        | (bes, _) ->
            nom && saadan ()
    
    (tal, koen, nled)
