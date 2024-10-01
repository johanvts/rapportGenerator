module LagkageGenerator

open System
open Basis

let opretLagkage (height: float) (andele: float list) (tekster: ordsek list) rotation : string =
    let total = List.sum andele
    let radianer = andele |> List.map(fun a ->  (Math.PI/180.0) * 360.0 * a/ total )
    let farver = ["#dc6b6a"; "#71b5e5"; "#c1c1ad"; "#abdc9b"; "#3b9c4b"; "#ed8822"]
    let kageStykke origo p1 p2 farve omvej =
        sprintf """<path d="M %f,%f L %f,%f A%f,%f 0 %d,1 %f,%f Z" fill="%s" stroke="black" />"""
            (fst origo) (snd origo)// M - Move to
            (fst p1) (snd p1) // L - Line to
            (height / 2.0) (height / 2.0) omvej (fst p2) (snd p2) // A - rx ry large arc? destination dx, dy
            farve // Fill color
    let forklaring farve (nummer:int) tekst =
        let rektangelHoejde = height/(4.0 * (float)(tekster |> List.length))
        let (x,y) = (height + 50.0 , ((rektangelHoejde + 5.0) * (float)nummer))
        sprintf """<rect width="%f" height="%f" x="%f" y="%f" rx="3" ry="3" fill="%s" stroke="black"/>\n\
                   <text x="%f" y="%f">%s</text>"""
            (2.0*rektangelHoejde) rektangelHoejde x y farve
            (x + (2.0 * rektangelHoejde) + 5.0) (y+0.6*rektangelHoejde) tekst
    let rec kagebager (acc: string list) (currentAngle: float) (radiansList: float list) (textList: ordsek list) (colorList: string list) =
        match radiansList, textList, colorList with
            | [], _, _ | _, [], _ | _, _, [] -> acc
            | r::rs, t::ts, c::cs ->
                let x1 = height / 2.0 + (height / 2.0) * Math.Cos(currentAngle)
                let y1 = height / 2.0 + (height / 2.0) * Math.Sin(currentAngle)
                let x2 = height / 2.0 + (height / 2.0) * Math.Cos(currentAngle + r)
                let y2 = height / 2.0 + (height / 2.0) * Math.Sin(currentAngle + r)
                let omvej = if r > Math.PI then 1 else 0
                let stykke = kageStykke ((height / 2.0),(height / 2.0)) (x1, y1) (x2, y2) c omvej
                let stykkeForklaring = forklaring c ((List.length acc)/2) (Format.strengPrinter (Format.begyndelse t))
                kagebager (stykke::stykkeForklaring::acc) (currentAngle + r) rs ts cs
    let pieChartSvg = String.concat "\n" (kagebager [] rotation radianer tekster farver)

    sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\"  width=\"%f\" height=\"%f\" viewbox = \"0 -5 %f %f\">\n%s\n</svg>" (height*2.0) (height+10.0) (height*2.0) (height + 10.0) pieChartSvg
