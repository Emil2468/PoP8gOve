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
