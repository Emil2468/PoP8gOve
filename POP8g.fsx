// POP8g Øvelsesopgaver

// 8gØ.0

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
