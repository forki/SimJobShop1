module SimJobShop.Engine

/// Represents a command that is scheduled to be excecuted in the future.
/// Implements IComparable (CustomComparison) based on the comparison of the time property.
/// CustomEquality is implemented for the comparisons <= and >=.
[<CustomEquality; CustomComparison>]
type Command<'Time, 'CommandAction when 'Time : comparison and 'CommandAction : equality> = 
    { Time : 'Time
      Action : 'CommandAction }
    
    override this.Equals obj = 
        match obj with
        | :? Command<'Time, 'CommandAction> as other -> (this.Time = other.Time) && (this.Action = other.Action)
        | _ -> false
    
    override x.GetHashCode() = hash x
    interface System.IComparable with
        member this.CompareTo obj = 
            match obj with
            | :? Command<'Time, 'CommandAction> as other -> compare this.Time other.Time
            | _ -> invalidArg "yobj" "cannot compare value of different types"

/// Represents an Schedule of future commands, i.e. upcoming events.
type Schedule<'Command when 'Command : comparison> = 
    | Schedule of 'Command list

module Schedule = 
    /// The generic empty Schedule
    let Empty = Schedule []
    
    /// Return the next command and the remaining schedule.
    let Take schedule = 
        match schedule with
        | Schedule [] -> None, schedule
        | Schedule(head :: tail) -> Some head, Schedule tail
    
    /// Add a new command to the schedule by recreating the schedule list from the tail 
    /// and inserting new command just after the last command with the same timestamp.
    let Add command schedule = 
        match schedule with
        | Schedule [] -> Schedule [ command ]
        | Schedule list -> 
            let folder x accList = 
                match accList with
                | [] -> [ x ]
                | [ head ] -> 
                    if x < head then [ x; head ]
                    else [ head; x ]
                | head :: tail -> 
                    if x < head then x :: head :: tail
                    else head :: x :: tail
            List.foldBack folder list [ command ] |> Schedule



// Testing the schedule
let c1 = { Time=0; Action="create"}
let c2 = { Time=1; Action="move"}
let c3 = { Time=2; Action="exit"}
let s =
    Schedule.Empty
    |> Schedule.Add c3
    |> Schedule.Add c1
    |> Schedule.Add c2
let co1, s1 = Schedule.Take s
let co2, s2 = Schedule.Take s1
let co3, s3 = Schedule.Take s2
let check = [c1=co1.Value; c2=co2.Value; c3=co3.Value; s3=Schedule.Empty]




type Simulation<'Time,'Model,'Command when 'Command : comparison> = {
    Time : 'Time
    State : 'Model
    Schedule : Schedule<'Command>
    }

