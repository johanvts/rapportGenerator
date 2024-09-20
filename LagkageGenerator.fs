module LagkageGenerator

open System

let createPieChart (height: float) (andele: float list) (tekster: string list) : string =
    let total = List.sum andele
    let angles = andele |> List.map (fun a -> 360.0 * (a / total))
    let radians = angles |> List.map (fun a -> (Math.PI / 180.0) * a)
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
                let path = sprintf "<path d=\"M %f,%f L %f,%f A%f,%f 0 %d,1 %f,%f Z\" fill=\"%s\" stroke=\"black\" />" 
                                (height / 2.0) (height / 2.0) x1 y1 (height / 2.0) (height / 2.0) largeArcFlag x2 y2 c
                pieHelper (path::acc) (currentAngle + r) rs ts cs

        pieHelper [] 0.0 radians tekster colors

    let pieChartSvg = String.concat "\n" (pieSlices)

    sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\"  width=\"%f\" height=\"%f\">\n%s\n</svg>" height height pieChartSvg
