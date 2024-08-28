module Smaaord

open Basis

let artikel = function
  | ((Ubs, Ent), Itk) -> Str "et"
  | ((Bes, Ent), Itk) -> Str "det"
  | ((Ubs, Ent), Fk ) -> Str "en"
  | ((Bes, Ent), Fk ) -> Str "den"
  | ((Ubs, Flt), _  ) -> Tom
  | ((Bes, Flt), _  ) -> Str "de"

let adverbier =
    [|"ekstremt"; "ofte"; "helt"; "klart";
      "meget"; "muligvis"; "normalt";
      "tilstrækkeligt"; "typisk"; "udpræget"; "udtalt"|]

let vaelgadv () = Str (vaelgfra adverbier)
