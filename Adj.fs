module Adj

open Basis

type adjektiv =
    | RegA of string * string * string
         (* fk/ent   itk/ent  flt+bes *)

let adjektiver = 
     [RegA("aktiv", "-t", "-e"),
      RegA("aktuel", "-t", "-le"),
      RegA("analytisk", "-", "-e"),
      RegA("anerkendt", "-", "-e"),
      RegA("anvendt", "-", "-e"),
      RegA("basal", "-t", "-e"),
      RegA("central", "-t", "-e"),
      RegA("dansk", "-", "-e"),
      RegA("dynamisk", "-", "-e"),
      RegA("empirisk", "-", "-e"),
      RegA("etisk", "-", "-e"),
      RegA("forbedret", "-", "forbedrede"),
      RegA("frugtbar", "-t", "-e"),
      RegA("f�lles", "-", "-"),
      RegA("generel", "-t", "-le"),
      RegA("global", "-t", "-e"),
(*    RegA("god", "-t", "-e"), *)
      RegA("heuristisk", "-", "-e"),
      RegA("hierarkisk", "-", "-e"),
      RegA("horisontal", "-t", "-e"),
      RegA("humanistisk", "-", "-e"),
      RegA("individuel", "-t", "-le"),
      RegA("innovativ", "-t", "-e"),
      RegA("isoleret", "-", "isolerede"),
      RegA("integreret", "-", "integrerede"),
      RegA("international", "-t", "-e"),
      RegA("kommunikerbar", "-t", "-e"),
      RegA("kompetent", "-", "-e"),
      RegA("kompleks", "-t", "-e"),
      RegA("konceptuel", "-t", "-le"),
      RegA("konkret", "-", "-e"),
      RegA("koordineret", "-", "koordinerede"),
      RegA("kvalitativ", "-t", "-e"),
      RegA("langsigtet", "-", "langsigtede"),
      RegA("lokal", "-t", "-e"),
      RegA("metodisk", "-", "-e"),
      RegA("moderne", "-", "-"),
      RegA("netv�rksbaseret", "-", "netv�rksbaserede"),
      RegA("offentlig", "-t", "-e"),
      RegA("organisatorisk", "-", "-e"),
      RegA("ny", "-t", "-e"),
      RegA("passiv", "-t", "-e"),
      RegA("permanent", "-", "-e"),
      RegA("politisk", "-", "-e"),
      RegA("privat", "-", "-e"),
      RegA("problematisk", "-", "-e"),
      RegA("projektorienteret", "-", "projektorienterede"),
      RegA("relevant", "-", "-e"),
      RegA("samfundsvidenskabelig", "-t", "-e"),
      RegA("samfundsm�ssig", "-t", "-e"),
      RegA("social", "-t", "-e"),
      RegA("speciel", "-t", "-le"),
      RegA("specifik", "-t", "-ke"),
      RegA("strategisk", "-", "-e"),
      RegA("synlig", "-t", "-e"),
      RegA("s�rlig", "-t", "-e"),
      RegA("teoretisk", "-", "-e"),
      RegA("traditionel", "-t", "-le"),
      RegA("trov�rdig", "-t", "-e"),
      RegA("tv�rfaglig", "-t", "-e"),
      RegA("udenlandsk", "-", "-e"),
      RegA("velstruktureret", "-", "velstrukturerede"),
      RegA("vertikal", "-t", "-e"),
      RegA("vidensbaseret", "-", "vidensbaserede"),
      RegA("videnskabelig", "-t", "-e"),
      RegA("vigtig", "-t", "-e"),
      RegA("virtuel", "-t", "-le"),
      RegA("�konomisk", "-", "-e")];         

let boej (RegA(fkent, itkent, flt_bes)) form koen =
    match form, koen with
    | (ubs, ent), fk -> fkent
    | (ubs, ent), itk -> tilfoej fkent itkent
    | _ -> tilfoej fkent flt_bes

let vaelg () = vaelgfra adjektiver

let vaelgikke forbudt = vaelgikke forbudt adjektiver
