// POP8g Øvelsesopgaver

// 8gØ.0
#r "img_util.dll"

type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

let dayToNumber (day: weekday) : int =
    match day with
    | Monday -> 1
    | Tuesday -> 2
    | Wednesday -> 3
    | Thursday -> 4
    | Friday -> 5
    | Saturday -> 6
    | Sunday -> 7
printfn "dayToNumber Monday = %A" (dayToNumber Monday)
printfn "dayToNumber Tuesday = %A" (dayToNumber Tuesday)
printfn "dayToNumber Wednesday = %A" (dayToNumber Wednesday)
printfn "dayToNumber Thursday = %A" (dayToNumber Thursday)
printfn "dayToNumber Friday = %A" (dayToNumber Friday)
printfn "dayToNumber Saturday = %A" (dayToNumber Saturday)
printfn "dayToNumber Sunday = %A" (dayToNumber Sunday)

// 8gø.1

let nextDay (day : weekday) : weekday =
    match day with
    | Monday -> Tuesday
    | Tuesday -> Wednesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Saturday
    | Saturday -> Sunday
    | Sunday -> Monday

printfn "Test af nextDay"
printfn "nextDay Monday = %A" (nextDay Monday)
printfn "nextDay Tuesday = %A" (nextDay Tuesday)
printfn "nextDay Wednesday = %A" (nextDay Wednesday)
printfn "nextDay Thursday = %A" (nextDay Thursday)
printfn "nextDay Friday = %A" (nextDay Friday)
printfn "nextDay Saturday = %A" (nextDay Saturday)
printfn "nextDay Sunday = %A" (nextDay Sunday)

// 8gø.2

type point = int * int
type colour = int * int * int

type figure =
    | Circle of point * int * colour
    | Rectangle of point * point * colour
    | Mix of figure * figure

let rec  colourAt (x,y) figure =
    match  figure  with
    | Circle  ((cx,cy), r, col) ->
        if (x-cx)*(x-cx)+(y-cy)*(y-cy) <= r*r
        then  Some  col  else  None
    | Rectangle  ((x0,y0), (x1,y1), col) ->
        if x0 <=x && x <= x1 && y0  <= y && y <= y1
        then  Some  col  else  None
    | Mix (f1, f2) ->
        match (colourAt (x,y) f1, colourAt (x,y) f2) with
        | (None , c) -> c   // no  overlap
        | (c, None) -> c   // no  overlap
        | (Some (r1,g1,b1), Some (r2,g2,b2)) ->
            Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2)