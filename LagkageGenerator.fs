module LagkageGenerator

open System

let createPieChart (height: float) (andele: float list) (tekster: string list) (rng:System.Random) : string =
    let total = List.sum andele
    let radians = andele |> List.map(fun a ->  (Math.PI/180.0) * 360.0 * a/ total )
    let colors = ["#dc6b6a"; "#71b5e5"; "#c1c1ad"; "#abdc9b"; "#3b9c4b"; "#ed8822"]

    let pieSlices =
        let rec pieHelper (acc: string list) (currentAngle: float) (radiansList: float list) (textList: string list) (colorList: string list) =
            match radiansList, textList, colorList with
            | [], _, _ | _, [], _ | _, _, [] -> acc
            | r::rs, t::ts, c::cs ->
                let x1 = height / 2.0 + (height / 2.0) * Math.Cos(currentAngle)
                let y1 = height / 2.0 + (height / 2.0) * Math.Sin(currentAngle)
                let x2 = height / 2.0 + (height / 2.0) * Math.Cos(currentAngle + r)
                let y2 = height / 2.0 + (height / 2.0) * Math.Sin(currentAngle + r)
                let largeArcFlag = if r > Math.PI then 1 else 0
                let path = sprintf """<path d="M %f,%f L %f,%f A%f,%f 0 %d,1 %f,%f Z" fill="%s" stroke="black" />\n\
                <text x="%f" y="%f">%s</text>""" 
                                (height / 2.0) (height / 2.0) // M - Move to
                                x1 y1 // L - Line to
                                (height / 2.0) (height / 2.0) largeArcFlag x2 y2 // A - rx ry large arc? destination dx, dy
                                c // Fill color
                                x2 y2 // Text location
                                t// Text
                pieHelper (path::acc) (currentAngle + r) rs ts cs

        pieHelper [] (rng.Next(8)) radians tekster colors

    let pieChartSvg = String.concat "\n" (pieSlices)

    sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\"  width=\"%f\" height=\"%f\">\n%s\n</svg>" height height pieChartSvg
