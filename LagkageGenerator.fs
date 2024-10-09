module LagkageGenerator

open System
open Basis

let forklaring x farve (nummer:int) tekst =
    let rektangelHoejde = 35.
    let (x,y) = (x + 50.0 , ((rektangelHoejde + 10.0) * (float)nummer))
    sprintf """<rect width="%f" height="%f" x="%f" y="%f" rx="1" ry="1" fill="%s" stroke="black"/>\n\
        <text x="%f" y="%f">%s</text>"""
            (2.0*rektangelHoejde) rektangelHoejde x y farve
            (x + (2.0 * rektangelHoejde) + 5.0) (y+0.6*rektangelHoejde) tekst        

let opretLagkage (hoejde: float) (tekster: ordsek list) rotation : string =
    let andele = tekster |> List.map (fun _ -> (float)(terning 100))
    let total = List.sum andele
    let radianer = andele |> List.map(fun a ->  (Math.PI/180.0) * 360.0 * a/ total )
    let farver = ["#32D0E5"; "#2745CF"; "#21BC55"; "#742C93"; "#02080B"; "#FFD800"]
    let kageStykke origo p1 p2 farve omvej =
        sprintf """<path d="M %f,%f L %f,%f A%f,%f 0 %d,1 %f,%f Z" fill="%s" stroke="black" />"""
            (fst origo) (snd origo)// M - Move to
            (fst p1) (snd p1) // L - Line to
            (hoejde / 2.0) (hoejde / 2.0) omvej (fst p2) (snd p2) // A - rx ry large arc? destination dx, dy
            farve // Fill color

    let rec kagebager (acc: string list) (currentAngle: float) (radiansList: float list) (textList: ordsek list) (colorList: string list) =
        match radiansList, textList, colorList with
            | [], _, _ | _, [], _ | _, _, [] -> acc
            | r::rs, t::ts, f::fs ->
                let x1 = hoejde / 2.0 + (hoejde / 2.0) * Math.Cos(currentAngle)
                let y1 = hoejde / 2.0 + (hoejde / 2.0) * Math.Sin(currentAngle)
                let x2 = hoejde / 2.0 + (hoejde / 2.0) * Math.Cos(currentAngle + r)
                let y2 = hoejde / 2.0 + (hoejde / 2.0) * Math.Sin(currentAngle + r)
                let omvej = if r > Math.PI then 1 else 0
                let stykke = kageStykke ((hoejde / 2.0),(hoejde / 2.0)) (x1, y1) (x2, y2) f omvej
                let stykkeForklaring = forklaring hoejde f ((List.length acc)/2) (Format.strengPrinter (Format.begyndelse t))
                kagebager (stykke::stykkeForklaring::acc) (currentAngle + r) rs ts fs
    let pieChartSvg = String.concat "\n" (kagebager [] rotation radianer tekster farver)

    sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\"  width=\"%f\" height=\"%f\" viewbox = \"0 -5 %f %f\">\n%s\n</svg>" (hoejde*2.0) (hoejde+10.0) (hoejde*2.0) (hoejde + 10.0) pieChartSvg


let opretKurver (hoejde:float) (tekster: ordsek list) =
    let laengde = hoejde * 2.0
    let xAkse =
        sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="black" stroke-width="2"/>\n\
                   <polygon points="%f,%f,%f,%f,%f,%f" fill="black"/>"""
           10.0 (hoejde) (laengde) (hoejde)
           (laengde) hoejde (laengde-10.) (hoejde-5.) (laengde - 10.) (hoejde + 5.)
    let yAkse =
        sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f" stroke="black" stroke-width="2"/>\n\
                   <polygon points="%f,%f,%f,%f,%f,%f" fill="black"/>"""
           10f (hoejde) 10f 0f
           10f 0f 5f 10f 15f 10f
    let overKurve grundKurve startHop =
        let skridtLaengde = (laengde-10.)/(float)(grundKurve |> List.length)        
        grundKurve |> List.scan (fun (x,y) (nuY ) -> (x + skridtLaengde, Math.Max(10, Math.Min(nuY, y + (if terning 6 > 3 then -1.*(float)(terning 20) else (terning 20)))))) (10., (List.head grundKurve) - startHop)
    let svgKurve kurve farve =        
        sprintf """<polygon points="%f,%f,%f,%f,%s" fill="%s"/>"""
            laengde hoejde 10.0 hoejde (String.Join( ',',kurve |> List.map (fun (x,y) -> $"{x},{y}"))) farve
    let grundKurve = List.init 100 (fun _ -> (0., hoejde))
    let farver = ["#7C1E21";"#F7F036";"#066B35"; "#92A5AF";"#0095A0";"#BC352B"]
    let antalKurver = List.length tekster
    let svgKurver =
        [1..antalKurver] |> List.scan (fun kurve _ -> overKurve (List.map snd kurve) ((float)(terning 120))) grundKurve |> List.skip 1
        |> List.zip (farver |> List.sortBy (fun _ -> terning 5) |> List.take antalKurver) |> List.mapi (fun i (farve, kurve) -> (svgKurve kurve farve) + (forklaring (2.*hoejde) farve (antalKurver - i) (Format.strengPrinter (Format.begyndelse tekster.[i])))) |> List.rev |> String.Concat
    sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\"  width=\"%f\" height=\"%f\" viewbox = \"0 -5 %f %f\">\n%s\n</svg>" (hoejde*4.0) (hoejde+10.0) (hoejde*2.0) (hoejde + 10.0) ($"{svgKurver} {xAkse} {yAkse} ")
