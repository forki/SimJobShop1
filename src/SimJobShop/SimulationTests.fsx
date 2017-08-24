#load "Common.fs"
#load "Engine.fs"

open SimJobShop.Common
open SimJobShop.Engine

type Event<'Time, 'Fact> = 
    { Time : 'Time
      Fact : 'Fact }

// Testing a dummy model
type DummyState = 
    { Time : int
      Number : int }

type DummyAction = 
    | Add
    | Subtract

type DummyCommand = Command<int, DummyAction>

type DummyFact = 
    | Added
    | Subtracted

let dummyExecute (state : DummyState) (command : DummyCommand) = 
    match command with
    | { Time = time; Action = Add } -> 
        [ { Time = time
            Fact = Added } ]
    | { Time = time; Action = Subtract } -> 
        [ { Time = time
            Fact = Subtracted } ]

let dummyApply (state : DummyState) event = 
    let newState = 
        match event with
        | { Time = time; Fact = Added } -> 
            { state with Time = time
                         Number = state.Number + 1 }
        | { Time = time; Fact = Subtracted } -> 
            { state with Time = time
                         Number = state.Number - 1 }
    (newState, [])

let dummyModel = 
    { InitialState = 
          { Time = 0
            Number = 0 }
      Execute = dummyExecute
      Apply = dummyApply }

let saveEvent = ignore
let log = printfn "%s"
let step sim = Simulation.evolveSim saveEvent log sim

//let maxTime = 10
//let rand = System.Random(1)
//let n = rand.Next(10)
let initialSchedule = 
    [ { Time = 1
        Action = Add }
      { Time = 2
        Action = Subtract } ]
    |> Schedule.ofSeq

let sim0 = Simulation.createInitial dummyModel initialSchedule
let sim1 = step sim0
let sim2 = step sim1
let sim3 = step sim2
let sim4 = Simulation.run saveEvent log sim0
