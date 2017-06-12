open System

#load "Common.fs"
open SimJobShop.Common

//#load "EventStore.fs"
//open SimJobShop.EventStore

#load "JobShopData.fs"
open SimJobShop.JobShopData

#load "Engine.fs"
open SimJobShop.Engine

#load "JobShopModel.fs"
open SimJobShop.JobShopModel


// ======================================
// Test data
// ======================================
let d0 = JobShopData.create () |> JobShopData.addDefaultMachines 2
let machineIds = JobShopData.getAllMachineIds d0
//let r = JobShopData.writeMachinesToFile "\t" """c:\temp\machines.txt""" d
let m1 = machineIds |> Seq.head
let m2 = machineIds |> Seq.skip 1 |> Seq.head
let t1 = JobShopData.makeTask (m1, 0u, TimeSpan.FromHours(1.0), FiniteCapacity 1u) d0
let t2 = JobShopData.makeTask (m2, 1u, TimeSpan.FromHours(2.0), FiniteCapacity 1u) d0
//let p1, d1 = JobShopData.makeProduct ([t1; t2; {t1 with Rank=2u}], 2.5, 250u) d0
let p1, d1 = JobShopData.makeProduct ([t1; t2], 2.5, 250u) d0
let p2, d2 = JobShopData.makeProduct ([{t2 with Rank=0u}; {t1 with Rank=1u}], 3.0, 120u) d1


// some jobs
let data = 
    d2
    |> JobShopData.makeJob (p1, DateTime.Today, DateTime.Today.AddDays(2.0)) |> snd
    |> JobShopData.makeJob (p1, DateTime.Today, DateTime.Today.AddDays(2.0)) |> snd

// write to files
JobShopData.writeDataToFiles """C:\Temp\testSim""" data


// ======================================
// Simulation
// ======================================
let mutable eventsLog = List.empty<Event>
let saveEvent event = eventsLog <- event::eventsLog
let log = printfn "%s"
let step sim = Simulation.evolveSim saveEvent log sim

let initial = initSimulation data

//let sim1 = step initial
//let sim2 = step sim1
//let sim3 = step sim2
//let sim4 = step sim3
//let sim5 = step sim4
//let sim6 = step sim5
//let sim7 = step sim6
//let sim8 = step sim7
//let sim9 = step sim8
//let sim10 = step sim9

let final = Simulation.run saveEvent log initial

//let r = eventsLog |> Seq.rev |> Event.writeEventsToFile "\t" """C:\Temp\testSim\events.txt""" 
let r = eventsLog |> Seq.rev |> Event.writeEventsToFile "\t" """C:\Temp\test\events.txt""" 


(*******************

Event '{Time = 12.06.2017 00:00:00;
 Fact = EntityCreated (Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = EntityCreated (Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = EntityEnteredInWaitlist (Id 1UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = EntityEnteredInWaitlist (Id 2UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = EntitySelectedFromWaitlist (Id 1UL,Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = CapacityBlocked (Id 1UL,Id 1UL,FiniteCapacity 1u);}' generated 0 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = ChangeoverStarted (Id 1UL,Id 1UL,00:30:00);}' generated 1 commands
Event '{Time = 12.06.2017 00:30:00;
 Fact = ChangeoverEnded (Id 1UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:30:00;
 Fact = MovedToLocation (Id 1UL,Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 00:30:00;
 Fact = ProcessingStarted (Id 1UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = ProcessingEnded (Id 1UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = EntityEnteredInWaitlist (Id 1UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = EntitySelectedFromWaitlist (Id 1UL,Id 2UL);}' generated 0 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = CapacityBlocked (Id 1UL,Id 2UL,FiniteCapacity 1u);}' generated 0 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = ChangeoverStarted (Id 1UL,Id 2UL,00:30:00);}' generated 1 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = ChangeoverEnded (Id 1UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = MovedToLocation (Id 1UL,Id 2UL);}' generated 0 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = CapacityReleased (Id 1UL,Id 1UL,FiniteCapacity 1u);}' generated 1 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = ProcessingStarted (Id 1UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = EntitySelectedFromWaitlist (Id 2UL,Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = CapacityBlocked (Id 2UL,Id 1UL,FiniteCapacity 1u);}' generated 0 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = ChangeoverStarted (Id 2UL,Id 1UL,00:30:00);}' generated 1 commands
Event '{Time = 12.06.2017 02:30:00;
 Fact = ChangeoverEnded (Id 2UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 02:30:00;
 Fact = MovedToLocation (Id 2UL,Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 02:30:00;
 Fact = ProcessingStarted (Id 2UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 03:30:00;
 Fact = ProcessingEnded (Id 2UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 03:30:00;
 Fact = EntityEnteredInWaitlist (Id 2UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = ProcessingEnded (Id 1UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = CapacityReleased (Id 1UL,Id 2UL,FiniteCapacity 1u);}' generated 1 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = EntityAnnihilated (Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = EntitySelectedFromWaitlist (Id 2UL,Id 2UL);}' generated 0 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = CapacityBlocked (Id 2UL,Id 2UL,FiniteCapacity 1u);}' generated 0 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = ChangeoverStarted (Id 2UL,Id 2UL,00:30:00);}' generated 1 commands
Event '{Time = 12.06.2017 04:30:00;
 Fact = ChangeoverEnded (Id 2UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 04:30:00;
 Fact = MovedToLocation (Id 2UL,Id 2UL);}' generated 0 commands
Event '{Time = 12.06.2017 04:30:00;
 Fact = CapacityReleased (Id 2UL,Id 1UL,FiniteCapacity 1u);}' generated 1 commands
Event '{Time = 12.06.2017 04:30:00;
 Fact = ProcessingStarted (Id 2UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 06:30:00;
 Fact = ProcessingEnded (Id 2UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 06:30:00;
 Fact = CapacityReleased (Id 2UL,Id 2UL,FiniteCapacity 1u);}' generated 1 commands
Event '{Time = 12.06.2017 06:30:00;
 Fact = EntityAnnihilated (Id 2UL);}' generated 0 commands
Simulaten terminated: Schedule is empty




********************)













//type JobShopData = 
//    { Machines : Repository<Machine Id, Machine>
//      Products : Repository<Product Id, Product>
//      Jobs : Repository<Job Id, Job> }



(************************************************************************
type EntityId = EntityId of Guid
type LocationId = LocationId of Guid
type EntityTask = LocationId * TimeSpan
type EntityJob = {JobId : Job Id; Tasks : EntityTask list}
type EntityState = | Done | PendingTasks of EntityTask list
type Entity = {Id : EntityId; JobId : Job Id; State : EntityState }
type Location = {Id : LocationId; MachineId : Machine Id; CapacityTotal : Capacity; CapacityAvailable : Capacity; Waitlist : EntityId list}
type State = {Time : DateTime; Entities : Repository<EntityId,Entity>; Locations : Repository<LocationId,Location>}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    /// Create the initial state from the given JobShopData
    let empty =
        // let time = earliest release date jobs
        { Time = DateTime.Today
          Entities = Repository.create (fun () -> Guid.NewGuid() |> EntityId)
          Locations = Repository.create (fun () -> Guid.NewGuid() |> LocationId) }

    let createLocation (machine : Machine) (state : State) =
        let makeItem id =
            { Id = id
              MachineId = machine.Id
              CapacityTotal = machine.Capacity
              CapacityAvailable = machine.Capacity
              Waitlist = [] }
        let locationId, locationRepo = Repository.insert makeItem state.Locations
        (locationId, {state with Locations = locationRepo})
        

    let create jobShopData =
        Repository.getAllItems jobShopData.Machines
        |> Seq.fold (fun state machine -> createLocation machine state |> snd) empty


type CommandAction = 
    | CreateEntityForJob of Job Id

type EventFact = 
    // An entity with this Id was created and moved to the source
    | CreatedEntityForJob of Job Id

type Time = DateTime
type Command = Command<Time,CommandAction>
type Event = { Time : Time; Fact : EventFact }


let execute (state:State) (command:Command) : Event list =
    match command.Action with
    | CreateEntityForJob jobId -> []

let apply (state:State) (event:Event) : (State * Command list) =
    match event.Fact with
    | CreatedEntityForJob jobId -> state, []


************************************************************************)


(**
- locations represent machines, so they have
    - capacity
    - current load
    - an input buffer with
        - capacity
        - current load
- source location has infinite capacity and no input buffer
- sink location  has infinite capacity and no input buffer

- entities represent jobs, so they have
    - a list of locations to visit for given durations
    (- a list of locations that have been visited)


questions
- waiting lists for entities that want to move bu cannot yet?
    - location view:
        - after processing an entity in a location the entity is commanded 
            - either to enlist in the waiting list of the machine for the next task
            - or to move to the sink
        - the location has capacity available and commands the next entity from the waiting list to move to the location
    - when a location has capacity available it emits a command that the next entity from  its waiting list can be moved there.
- state? where to hold the entities?
- machine and buffer?
- how to know which location an entity is in?

**)

(**
let's ignore buffers for now 
//type Buffer = {MachineId : MachineId; Capacity : Capacity; CapacityAvailable : Capacity}
//type Process = {MachineId : MachineId; Capacity : Capacity; CapacityAvailable : Capacity; InputBuffer of Buffer}
//type Location = | Source | Sink | Process of Process
**)


//type Waitinglist = 


