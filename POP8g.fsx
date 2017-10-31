/// POP8g Øvelsesopgaver

// 8gØ.0

type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

let nextDay (day : weekday) : weekday =
    match day with
    | Monday -> Tuesday
    | Tuesday -> Wendesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Saturday
    | Saturday -> Sunday
    | Sunday -> Monday
