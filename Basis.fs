module Basis

open System

type ordsek = 
    | Tom 
    | Str of string
    | OrdSeq of ordsek * ordsek

let (&&&) ordA ordB = OrdSeq(ordA, ordB)

let rec mksek n f =
    match n with
    | 0 -> Tom
    | _ -> f() &&& mksek (n-1) f

type genus = | Itk | Fk
type numerus = | Ent | Flt
type bestemthed = | Bes | Ubs

exception Umulig of string

(* Dan afledte former: ""= umulig, "-s" = tilfoej suffix s *)

let tilfoej rod suf =
    match suf with
    | "" -> raise (Umulig $"Umulig: {rod}")
    | _ when suf.[0] = '-' -> rod + suf.Substring(1)
    | _ -> suf

(* Generel valgfunktion*)

let mutable private seed = 0.0
let mutable private randgen = Random()

let private normaliser (rnd : float) =
    float (int (rnd * 2147483647.0 + -1073741824.0)) + 1073741824.0

let private initialiser s =
    seed <- s
    randgen <- Random(int s)

let strseed () = seed.ToString("F0")
let skrivseed () = Str (strseed())
let saetseed s = initialiser s

let vaelgfra (vec: 'a array) = 
    vec.[randgen.Next(vec.Length)]

let rec vaelgikke forbudt vec =
    let res = vaelgfra vec
    if List.exists ((=) res) forbudt then 
        vaelgikke forbudt vec 
    else
        res

let terning n = randgen.Next(1, n + 1)

let ssh r = randgen.NextDouble() < r
