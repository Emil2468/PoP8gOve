/// POP8g Øvelsesopgaver

// 8gØ.0

type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

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
