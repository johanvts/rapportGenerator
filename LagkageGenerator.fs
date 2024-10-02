module LagkageGenerator

open System
open Basis

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
    let forklaring farve (nummer:int) tekst =
        let rektangelHoejde = hoejde/(4.0 * (float)(tekster |> List.length))
        let (x,y) = (hoejde + 50.0 , ((rektangelHoejde + 10.0) * (float)nummer))
        sprintf """<rect width="%f" height="%f" x="%f" y="%f" rx="3" ry="3" fill="%s" stroke="black"/>\n\
                   <text x="%f" y="%f">%s</text>"""
            (2.0*rektangelHoejde) rektangelHoejde x y farve
            (x + (2.0 * rektangelHoejde) + 5.0) (y+0.6*rektangelHoejde) tekst
    let rec kagebager (acc: string list) (currentAngle: float) (radiansList: float list) (textList: ordsek list) (colorList: string list) =
        match radiansList, textList, colorList with
            | [], _, _ | _, [], _ | _, _, [] -> acc
            | r::rs, t::ts, c::cs ->
                let x1 = hoejde / 2.0 + (hoejde / 2.0) * Math.Cos(currentAngle)
                let y1 = hoejde / 2.0 + (hoejde / 2.0) * Math.Sin(currentAngle)
                let x2 = hoejde / 2.0 + (hoejde / 2.0) * Math.Cos(currentAngle + r)
                let y2 = hoejde / 2.0 + (hoejde / 2.0) * Math.Sin(currentAngle + r)
                let omvej = if r > Math.PI then 1 else 0
                let stykke = kageStykke ((hoejde / 2.0),(hoejde / 2.0)) (x1, y1) (x2, y2) c omvej
                let stykkeForklaring = forklaring c ((List.length acc)/2) (Format.strengPrinter (Format.begyndelse t))
                kagebager (stykke::stykkeForklaring::acc) (currentAngle + r) rs ts cs
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
    let kurve start skridt farve =
        let skridtLaengde = (int)laengde/skridt
        let udgangspunkt = (10, start)
        let svgPunkter = [0..skridt] |> List.scan (fun (x,y) _ -> (x + skridtLaengde, y + (if terning 6 > 3 then -1*(terning 10) else (terning 10)))) udgangspunkt
        sprintf """<polygon points="%f,%f,%f,%f,%s" fill="%s"/>"""
            laengde hoejde 10.0 hoejde (String.Join( ',',svgPunkter |> List.map (fun (x,y) -> $"{x},{y}"))) farve
    let antalKurver = Math.Max(2, terning 6)
    let kurve1 = kurve (terning ((int)hoejde)) 200 "#7C1E21"
    let kurve2 = kurve (terning ((int)hoejde)) 200 "#F7F036"
    sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\"  width=\"%f\" height=\"%f\" viewbox = \"0 -5 %f %f\">\n%s\n</svg>" (hoejde*2.0) (hoejde+10.0) (hoejde*2.0) (hoejde + 10.0) ($"{xAkse} {yAkse} {kurve1} {kurve2}")
