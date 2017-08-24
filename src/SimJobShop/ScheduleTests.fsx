#load "Common.fs"
#load "Engine.fs"
open SimJobShop.Common
open SimJobShop.Engine

// Tests for Command
let ``Comparison of commands is done based on comparison of generic times``
    (t1:'Time) (t2:'Time) = 
        let c1 = { Time=t1; Action="t1" }
        let c2 = { Time=t2; Action="t2" }
        (c1 < c2) = (t1 < t2) && (c1 <= c2) = (t1 <= t2) && (c1 >= c2) = (t1 >= t2) && (c1 > c2) = (t1 > t2)
let ``Comparison of commands is done based on comparison of integer times``
    (t1:int) (t2:int) = 
        ``Comparison of commands is done based on comparison of generic times`` t1 t2
let ``Comparison of commands is done based on comparison of float times``
    (t1:float) (t2:float) = 
        ``Comparison of commands is done based on comparison of generic times`` t1 t2
let ``Comparison of commands is done based on comparison of DateTime times``
    (t1:System.DateTime) (t2:System.DateTime) = 
        ``Comparison of commands is done based on comparison of generic times`` t1 t2

let commandTests = [
    ``Comparison of commands is done based on comparison of integer times`` 1 2
    ``Comparison of commands is done based on comparison of integer times`` 2 2
    ``Comparison of commands is done based on comparison of float times`` 5.0 2.0
    ``Comparison of commands is done based on comparison of float times`` 0.0 0.0
    ``Comparison of commands is done based on comparison of DateTime times`` System.DateTime.Today System.DateTime.Today
    ``Comparison of commands is done based on comparison of DateTime times`` System.DateTime.Today System.DateTime.Now ]


// Tests for Schedule
let c1 = { Time = 0; Action = "create" }
let c2 = { Time = 1; Action = "move" }
let c3 = { Time = 2; Action = "exit" }
let s = Schedule.ofSeq [c1;c2;c3]
let cR1, s1 = Schedule.next s
let cR2, s2 = Schedule.next s1
let cR3, s3 = Schedule.next s2
let commEqual comm1 commR = 
    match commR with
    | Success comm2 -> comm1 = comm2
    | Failure _ -> false
let check = [commEqual c1 cR1; commEqual c2 cR2; commEqual c3 cR3; Schedule.isEmpty s3]


let generateSomeCommands (n:uint32) =
    let count = (int)n
    let initializer t = { Time = t; Action = (sprintf "t=%i" t) }
    Seq.init count initializer

let ``Adding n commands one by one is equivalent to adding a list of commands in the same order``
    (n:uint32) =
        let commands = generateSomeCommands n
        let mutable s = Schedule.empty
        for c in commands do s <- Schedule.add s c
        let expected = s
        let actual = Schedule.ofSeq commands
        actual = expected

let ``Adding n commands and taking n commands yields an empty schedule``
    (n:uint32) =
        let mutable s = generateSomeCommands n |> Schedule.ofSeq
        let removeOneCommand schedule = 
            let _, newSchedule = Schedule.next schedule
            newSchedule
        for i in 1..(int)n do s <- removeOneCommand s
        Schedule.isEmpty s

let ``Adding two commands to an empty schedule then taking one command yields the earlier or first command``
    (t1:int) (t2:int) =
        let c1 = { Time = t1; Action = "first" }
        let c2 = { Time = t2; Action = "second" }
        let s = Schedule.ofSeq [ c1; c2 ]
        let commResult, _ = Schedule.next s
        let expected = if t1 <= t2 then c1 else c2
        match commResult with
        | Success command -> command = expected
        | Failure _ -> false


#time  // 10000 in ~ 30 sec
``Adding n commands one by one is equivalent to adding a list of commands in the same order`` 1000u
#time
#time  // 10000 in ~ 30 sec
``Adding n commands and taking n commands yields an empty schedule`` 1000u
#time
#time
[ ``Adding two commands to an empty schedule then taking one command yields the earlier or first command`` 1 0
  ``Adding two commands to an empty schedule then taking one command yields the earlier or first command`` 1 1
  ``Adding two commands to an empty schedule then taking one command yields the earlier or first command`` 1 2 ]
#time


// Performance of Schedule
let measurePerformanceOfAddingCommands (n:uint32) = 
    let commands = generateSomeCommands n
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    Schedule.ofSeq commands |> ignore
    stopWatch.Stop()
    printfn "%f ms" stopWatch.Elapsed.TotalMilliseconds
#time
measurePerformanceOfAddingCommands 10000u
#time
//13699.724600 ms
//Real: 00:00:13.715, CPU: 00:00:13.750, GC gen0: 716, gen1: 2, gen2: 1
