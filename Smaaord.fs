Module Smaaord

open Basis

let artikel = function
  | ((ubs, ent), itk) -> Str "et"
  | ((bes, ent), itk) -> Str "det"
  | ((ubs, ent), fk ) -> Str "en"
  | ((bes, ent), fk ) -> Str "den"
  | ((ubs, flt), _  ) -> Tom
  | ((bes, flt), _  ) -> Str "de"

let adverbier =
    [|"ekstremt", "ofte", "helt", "klart",
      "meget", "muligvis", "normalt",
      "tilstrækkeligt", "typisk", "udpræget", "udtalt"|]

let vaelgadv () = Str (vaelgfra adverbier)
