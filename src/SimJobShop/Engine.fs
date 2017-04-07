module SimJobShop.Engine

open System

/// Represents a command that is scheduled to be excecuted in the future.
type UpcomingCommand<'Time, 'Command when 'Time : comparison> = 
    { Time : 'Time
      Command : 'Command }

let upcomingCommandComparer c1 c2 = 
    if c1.Time > c2.Time then 1
    else if c1.Time < c2.Time then -1
    else 0

let isBefore upCommand1 upCommand2 = upCommand1.Time < upCommand2.Time

/// Represents an Schedule of future commands, i.e. upcoming events.
type Schedule<'UpcomingCommand> = 
    | Schedule of 'UpcomingCommand list

/// Return the next command and the remaining schedule.
let takeCommand schedule = 
    match schedule with
    | Schedule [] -> None, schedule
    | Schedule(head :: tail) -> Some head, Schedule tail

//TODO Ensure that this is tail recursive and make it a binary search!
/// Schedule a new command
let scheduleCommand upCommand schedule = 
    match schedule with
    | Schedule [] -> [ upCommand ]
    | Schedule list -> upCommand :: list |> List.sortWith upcomingCommandComparer
    |> Schedule
