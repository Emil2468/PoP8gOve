open System.Drawing
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

let (circle : figure) = (Circle ((50,50), 45, (255, 0, 0)))
let (rect : figure) = (Rectangle ((40, 40), (90, 110), (0,0,255)))
let (figTest : figure) = (Mix (circle, rect))

printfn "%A" figTest

// 8gØ.3 og 8gø.4

let makePicture (fileName: string) (fig: figure) (w: int) (h: int) : unit =
    let grey  = ImgUtil.fromRgb (128, 128, 128)
    let bmp = ImgUtil.mk w h
    for x = 0 to (w - 1) do
        for y = 0 to (h - 1) do
            match (colourAt (x,y) fig) with
            | None -> (ImgUtil.setPixel (grey) (x,y) bmp)
            | Some c -> (ImgUtil.setPixel (ImgUtil.fromRgb (c)) (x,y) bmp)
    ImgUtil.toPngFile fileName bmp
makePicture "test.png" figTest 100 150

// 8gØ.5

let checkCircle (Circle ((cx: int, cy: int), (rad: int), (r: int, b: int, g: int)) : figure) : bool =
    if rad > 0 && r >= 0 && r <= 255 && b >= 0 && b <= 255 && g >= 0 && g <= 255
        then true
    else
       false

let checkRect (Rectangle ((x0 : int,y0 : int), (x1 : int,y1 : int), (r : int, b : int, g : int)) : figure) : bool =
    let coords = (x1 - x0) > 0 && (y1 - y0) > 0
    let colors = r >= 0 && r < 256 && g >= 0 && g < 256 && b >= 0 && b < 256
    coords && colors

let rec checkFigure (fig : figure) : bool =
    match fig with
    | Circle  ((cx,cy), r, col) -> checkCircle fig
    | Rectangle ((x0,y0), (x1,y1), col) -> checkRect fig
    | Mix (f1, f2) -> checkFigure f1 && checkFigure f2

let badRect = Rectangle ((10, 10), (20, 20), (255, 0, 1))
let badCircle = Circle ((50,50), 45, (255, -1, 1))

printfn "%b" (checkFigure (Mix(badRect, badCircle)))

// 8gØ.6

let rec move (fig: figure) (x, y) =
    match fig with
    | Circle ((cx, cy), r, col) -> Circle (((cx + x), (cy + y)), r, col)
    | Rectangle ((x0, y0), (x1, y1), col) -> Rectangle (((x0 + x), (y0 + y)), ((x1 + x), (y1 + y)), col)
    | Mix (f1, f2) -> Mix(move f1 (x,y), move f2 (x,y))

makePicture "moveTest.png" (move figTest (-20,20)) 100 150

// 8gØ.7

let rec boundingBox (fig : figure) : (point * point) =
    match fig with
    | Circle ((cx, cy), r, col) -> ((cx - r, cy - r), ((cx + r), (cy + r)))
    | Rectangle ((x0, y0), (x1, y1), col) -> ((x0, y0), (x1, y1))
    | Mix (f1, f2) -> ((min (fst (fst (boundingBox f1))) (fst (fst(boundingBox f2))), (max (fst (snd (boundingBox f1))) (fst (snd (boundingBox f2))))),
                        (min (snd (fst(boundingBox f1))) (snd (fst (boundingBox f2))), (max (snd (snd (boundingBox f1))) (snd (snd (boundingBox f2))))))
let coords = boundingBox figTest
printfn "%A" coords
let (bound : figure) = (Rectangle ((fst coords, snd coords, (255,255,255))))
let boundTest = Mix(figTest, bound)
makePicture "bound.png" boundTest 100 150
