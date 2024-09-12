(* Forslag.sml * Copyright 1996 Peter Sestoft.  
   Må kopieres og modificeres jvfr. GNU General Public License *)

[<EntryPoint>]
let main argv =
    match argv with
    | [| |] -> Format.skaerm (Rapport.rapport ())
    | [| _; seed |] -> 
        match System.Double.TryParse seed with
        | true, r -> 
            Basis.saetseed r
            Format.skaerm (Rapport.rapport ())
        | false, _ -> printfn "Brug: forslag [rapportnr]"
    | _ ->         printfn "Brug: forslag [rapportnr]"
    
    0 // Return an integer exit code
