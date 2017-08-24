module SimJobShop.JobShopModelOutputBuffers

(*
This model avoids deadlocks by not blocking locations (occupying machines) 
while waiting for the next location to become available. Effectively, this 
means that locations have infinite capacity output buffers.
*)

open System
open Common
open Engine
open JobShopData

type EntityId = Job Id

type Entity = 
    { JobId : Job Id
      PendingTasks : Task list
      CurrentTask : Task option }

type LocationId = Machine Id

type Location = 
    { MachineId : Machine Id
      CapacityAvailable : Capacity
      CapacityTotal : Capacity
      Waitlist : EntityId list }

type State = 
    { Time : DateTime
      Data : JobShopData
      Entities : Map<EntityId, Entity>
      Locations : Map<LocationId, Location> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Entity = 
    let getNextTask entity = 
        match entity.PendingTasks with
        | [] -> None
        | t :: _ -> Some t
    
    let getNextMachineId entity = getNextTask entity |> Option.map (fun task -> task.MachineId)
    let getNextCapacityNeeded entity = getNextTask entity |> Option.map (fun task -> task.CapacityNeeded)
    
    let removeTask task entity = 
        entity.PendingTasks
        |> List.filter (fun t -> t <> task)
        |> fun tasklist -> { entity with PendingTasks = tasklist }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Location = 
    /// Select the next entity from the waitlist.
    /// TEMP: Strictly FIFO!
    let trySelectEntityFromWaitlist location = List.tryLast location.Waitlist
    
    let removeEntityFromWaitlist entityId location = 
        { location with Waitlist = location.Waitlist |> List.filter (fun id -> id <> entityId) }
    
    let isCapacityAvailable location entity = 
        Entity.getNextCapacityNeeded entity
        |> Option.map (Capacity.isGreaterOrEqual location.CapacityAvailable)
        |> function 
        | None -> false
        | Some b -> b
    
    /// Add the entity as head of the waitlist.
    let addToWaitlist entityId location = { location with Waitlist = entityId :: location.Waitlist |> List.distinct }
    
    let blockCapacity capacity location = 
        if Capacity.isGreater capacity location.CapacityAvailable then 
            sprintf "Inconsistency => capacity to be blocked is larger than available capacity: location id = %A" 
                location.MachineId |> failwith
        { location with CapacityAvailable = Capacity.subtract location.CapacityAvailable capacity }
    
    /// Release the given capacity at this location.
    /// Throws execption if resulting capacity is larger than total capacity.
    let releaseCapacity capacity location = 
        match Capacity.add location.CapacityAvailable capacity with
        | c when Capacity.isGreater c location.CapacityTotal -> 
            sprintf "Inconsistency => resulting capacity is larger than total capacity: location id = %A" 
                location.MachineId |> failwith
        | c -> { location with CapacityAvailable = c }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State = 
    let private create time jobShopData = 
        { Time = time
          Data = jobShopData
          Entities = Map.empty<EntityId, Entity>
          Locations = Map.empty<LocationId, Location> }
    
    let private addEntity state entity = 
        if Map.containsKey entity.JobId state.Entities then sprintf "Entity already exists: Job.Id = %A" id |> failwith
        { state with Entities = Map.add entity.JobId entity state.Entities }
    
    let private addLocation state location = 
        if Map.containsKey location.MachineId state.Locations then 
            sprintf "Location already exists: Machine.Id = %A" id |> failwith
        { state with Locations = Map.add location.MachineId location state.Locations }
    
    /// Create a new location for the given machine.
    /// This assumes that machine.Id is unique!
    let createLocation (machine : Machine) state = 
        { MachineId = machine.Id
          CapacityAvailable = machine.Capacity
          CapacityTotal = machine.Capacity
          Waitlist = [] }
        |> addLocation state
    
    let ofData jobShopData = 
        let time = JobShopData.getEarliestReleaseDate jobShopData
        let state0 = create time jobShopData
        JobShopData.getAllMachines jobShopData |> Seq.fold (fun state machine -> createLocation machine state) state0
    
    /// Create a new entity based on a given job.
    /// This assumes that job.Id is unique!
    let createEntity (jobId : Job Id) (state : State) = 
        let jobResult = JobShopData.getJob jobId state.Data
        let productResult = jobResult |> Result.bindR (fun job -> JobShopData.getProduct job.ProductId state.Data)
        Result.lift2R (fun job product -> 
            { JobId = job.Id
              PendingTasks = product.Tasks
              CurrentTask = None }) jobResult productResult
        |> Result.mapR (fun entity -> entity, addEntity state entity)
        |> Result.getValue
    
    let getEntity entityId state = 
        match Map.tryFind entityId state.Entities with
        | Some entity -> entity
        | None -> sprintf "Inconsistency: Entity not found! Id = %A" entityId |> failwith
    
    let getLocation locationId state = 
        match Map.tryFind locationId state.Locations with
        | Some location -> location
        | None -> sprintf "Inconsistency: Location not found! Id = %A" locationId |> failwith
    
    let getAllEntities state = 
        state.Entities
        |> Map.toSeq
        |> Seq.map snd
    
    let getAllLocations state = 
        state.Locations
        |> Map.toSeq
        |> Seq.map snd
    
    let updateLocation location state = 
        let id = location.MachineId
        if not (Map.containsKey id state.Locations) then 
            sprintf "Inconsistency => Location not found: location id = %A" id |> failwith
        { state with Locations = Map.add id location state.Locations }
    
    let updateEntity entity state = 
        let id = entity.JobId
        if not (Map.containsKey id state.Entities) then 
            sprintf "Inconsistency => Entity not found: entity id = %A" id |> failwith
        { state with Entities = Map.add id entity state.Entities }
    
    let deleteEntity entityId state = 
        if not (Map.containsKey entityId state.Entities) then 
            sprintf "Inconsistency => Entity not found: entity id = %A" id |> failwith
        { state with Entities = Map.remove entityId state.Entities }
    
    let getCapacityAvailable state = 
        getAllLocations state
        |> Seq.map (fun location -> location.CapacityAvailable)
        |> Capacity.sum
    
    let isCapacityAvailable state entity = 
        let available = getCapacityAvailable state
        Entity.getNextCapacityNeeded entity
        |> Option.map (Capacity.isGreaterOrEqual available)
        |> function 
        | None -> false
        | Some b -> b

type CommandAction = 
    | CreateEntity of Job Id
    | EnterEntityInWaitlist of EntityId * LocationId
    | TrySelectEntityFromWaitlist of LocationId
    //    | MoveToInputBuffer of EntityId * LocationId
    | MoveToLocation of EntityId * LocationId
    | EndChangeover of EntityId * LocationId
    | EndProcessing of EntityId * LocationId
    //    | MoveToOutputBuffer of EntityId * LocationId
    | AnnihilateEntity of EntityId

type EventFact = 
    | EntityCreated of Job Id
    | EntityEnteredInWaitlist of EntityId * LocationId
    | EntitySelectedFromWaitlist of EntityId * LocationId
    //    | EnteredInputBuffer of EntityId * LocationId
    //    | LeftInputBuffer of EntityId * LocationId
    | CapacityBlocked of EntityId * LocationId * Capacity
    | ChangeoverStarted of EntityId * LocationId * TimeSpan
    | ChangeoverEnded of EntityId * LocationId
    | MovedToLocation of EntityId * LocationId
    | ProcessingStarted of EntityId * LocationId
    | ProcessingEnded of EntityId * LocationId
    | CapacityReleased of EntityId * LocationId * Capacity
    //    | EnteredOutputBuffer of EntityId * LocationId
    //    | LeftOutputBuffer of EntityId * LocationId
    | EntityAnnihilated of EntityId

type Time = DateTime

type Event = 
    { Time : Time
      Fact : EventFact }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Event = 
    let csvHeader separator = [ "Time"; "Event"; "JobId"; "MachineId" ] |> String.concat separator
    
    let csvRecord separator (event : Event) = 
        let timeStr = event.Time.ToString()
        
        let factStr, jobIdStr, machineIdStr = 
            match event.Fact with
            | EntityCreated jobId -> "EntityCreated", Id.print jobId, "NA"
            | EntityEnteredInWaitlist(entityId, locationId) -> 
                "EntityEnteredInWaitlist", Id.print entityId, Id.print locationId
            | EntitySelectedFromWaitlist(entityId, locationId) -> 
                "EntitySelectedFromWaitlist", Id.print entityId, Id.print locationId
            | CapacityBlocked(entityId, locationId, _) -> "CapacityBlocked", Id.print entityId, Id.print locationId
            | ChangeoverStarted(entityId, locationId, _) -> "ChangeoverStarted", Id.print entityId, Id.print locationId
            | ChangeoverEnded(entityId, locationId) -> "ChangeoverEnded", Id.print entityId, Id.print locationId
            | MovedToLocation(entityId, locationId) -> "MovedToLocation", Id.print entityId, Id.print locationId
            | ProcessingStarted(entityId, locationId) -> "ProcessingStarted", Id.print entityId, Id.print locationId
            | ProcessingEnded(entityId, locationId) -> "ProcessingEnded", Id.print entityId, Id.print locationId
            | CapacityReleased(entityId, locationId, _) -> "CapacityReleased", Id.print entityId, Id.print locationId
            | EntityAnnihilated entityId -> "EntityAnnihilated", Id.print entityId, "NA"
        [ timeStr; factStr; jobIdStr; machineIdStr ] |> String.concat separator
    
    let writeEventsToFile separator (path : string) events = 
        try 
            use sw = new IO.StreamWriter(path)
            sw.WriteLine(csvHeader separator)
            events
            |> Seq.map (csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message

type Command = Command<Time, CommandAction>

let execute (state : State) (command : Command) : Event list = 
    let time = command.Time
    match command.Action with

    | CreateEntity entityData -> 
        [ { Time = time
            Fact = EntityCreated entityData } ]

    | EnterEntityInWaitlist(entityId, locationId) -> 
        [ { Time = time
            Fact = EntityEnteredInWaitlist(entityId, locationId) } ]

    | TrySelectEntityFromWaitlist locationId -> 
        let location = State.getLocation locationId state
        Location.trySelectEntityFromWaitlist location
        |> Option.map (fun entityId -> State.getEntity entityId state)
        |> Option.filter (fun entity -> Location.isCapacityAvailable location entity) //TEMP: May move this check to trySelect...
        |> Option.filter (fun entity -> State.isCapacityAvailable state entity)
        |> Option.toList
        |> List.collect (fun entity -> 
               [ EntitySelectedFromWaitlist(entity.JobId, locationId)
                 //TEMP: This may fail if there's no pending task:
                 CapacityBlocked(entity.JobId, locationId, entity.PendingTasks.Head.CapacityNeeded)
                 //TEMP: compute changeoverTime here and determine wether it is needed:
                 ChangeoverStarted(entity.JobId, locationId, TimeSpan.FromMinutes(30.0)) ])
        |> List.map (fun fact -> 
               { Time = time
                 Fact = fact })

    | EndChangeover(entityId, locationId) -> 
        [ { Time = time
            Fact = ChangeoverEnded(entityId, locationId) } ]

    | MoveToLocation(entityId, locationId) -> 
        //TEMP: improve this implementation
        let fact1 = MovedToLocation(entityId, locationId)
        //        let fact2option =
        //            State.getEntity entityId state
        //            |> fun entity -> entity.CurrentTask
        //            |> Option.map (fun task -> CapacityReleased (entityId, task.MachineId, task.CapacityNeeded) )///
        let fact3 = ProcessingStarted(entityId, locationId)
        [ //        [ Some fact1; fact2option; Some fact3 ]
          //        |> List.choose id
          fact1; fact3 ] |> List.map (fun fact -> 
                                { Time = time
                                  Fact = fact })

    | EndProcessing(entityId, locationId) -> 
        State.getEntity entityId state
        |> fun entity -> entity.CurrentTask
        |> Option.map (fun task -> 
               if task.MachineId <> locationId then failwith "Inconsistency => location and task.machineId don't match!"
               CapacityReleased(entityId, task.MachineId, task.CapacityNeeded))
        |> Option.toList
        |> fun facts -> (ProcessingEnded(entityId, locationId)) :: facts
        |> List.map (fun fact -> 
               { Time = time
                 Fact = fact })

    | AnnihilateEntity entityId -> 
        [ { //        let fact1option =
            //            State.getEntity entityId state
            //            |> fun entity -> entity.CurrentTask
            //            |> Option.map (fun task -> CapacityReleased (entityId, task.MachineId, task.CapacityNeeded) )
            //        let fact2 = EntityAnnihilated entityId
            //        [ fact1option; Some fact2 ]
            //        |> List.choose id
            //        |> List.map (fun fact -> { Time = time; Fact = fact } )
            Time = time
            Fact = EntityAnnihilated entityId } ]

//    | MoveToInputBuffer (entityId, locationId) -> EnteredInputBuffer (entityId, locationId)    
//    | MoveToOutputBuffer (entityId, locationId) -> EnteredOutputBuffer (entityId, locationId)


let apply state event = 
    let time = event.Time
    match event.Fact with

    | EntityCreated jobId -> 
        let entity, state' = State.createEntity jobId state
        
        let command = 
            match List.tryHead entity.PendingTasks with
            | Some task -> EnterEntityInWaitlist(entity.JobId, task.MachineId)
            | None -> AnnihilateEntity entity.JobId
            |> fun action -> 
                { Time = time
                  Action = action }
        ({ state' with Time = time }, [ command ])

    | EntityEnteredInWaitlist(entityId, locationId) -> 
        let location = State.getLocation locationId state
        let location' = Location.addToWaitlist entityId location
        let state' = State.updateLocation location' state
        
        let command = 
            { Time = time
              Action = TrySelectEntityFromWaitlist locationId }
        ({ state' with Time = time }, [ command ])

    | EntitySelectedFromWaitlist(entityId, locationId) -> 
        let location = State.getLocation locationId state
        let location' = Location.removeEntityFromWaitlist entityId location
        let state' = State.updateLocation location' state
        ({ state' with Time = time }, [])

    | CapacityBlocked(entityId, locationId, capacity) -> 
        let location = State.getLocation locationId state
        // TEMP: consistency checks
        if Capacity.isLess location.CapacityAvailable capacity then 
            sprintf 
                "Inconsistency => Not enough capacity available: entity id = %A, location id = %A, capacity available = %A, capacity needed = %A" 
                entityId locationId location.CapacityAvailable capacity |> failwith
        let location' = Location.blockCapacity capacity location
        let state' = State.updateLocation location' state
        ({ state' with Time = time }, [])

    | ChangeoverStarted(entityId, locationId, changeoverTime) -> 
        let command = 
            { Time = time.Add changeoverTime
              Action = EndChangeover(entityId, locationId) }
        ({ state with Time = time }, [ command ])

    | ChangeoverEnded(entityId, locationId) -> 
        let command = 
            { Time = time
              Action = MoveToLocation(entityId, locationId) }
        ({ state with Time = time }, [ command ])

    | MovedToLocation(_, _) -> ({ state with Time = time }, [])

    | ProcessingStarted(entityId, locationId) -> 
        let location = State.getLocation locationId state
        let entity = State.getEntity entityId state
        
        let task = 
            match Entity.getNextTask entity with
            | None -> 
                sprintf "Inconsistency => Entity has no task left: entity id = %A, location id = %A" entityId locationId 
                |> failwith
            | Some task when task.MachineId <> location.MachineId -> 
                sprintf "Inconsistency => wrong MachineId: expected = %A, actual = %A" id location.MachineId |> failwith
            | Some task -> task
        
        let entity' = { entity with CurrentTask = Some task }
        let state' = State.updateEntity entity' state
        
        let command = 
            { Time = time.Add task.ProcessingTime
              Action = EndProcessing(entityId, locationId) }
        ({ state' with Time = time }, [ command ])

    | ProcessingEnded(entityId, locationId) -> 
        let entity = State.getEntity entityId state
        
        let currentTask = 
            match entity.CurrentTask with
            | None -> 
                sprintf "Inconsistency => Entity has no current task: entity id = %A, location id = %A" entityId 
                    locationId |> failwith
            | Some task -> task
        
        let entity' = Entity.removeTask currentTask entity
        let state' = State.updateEntity entity' state
        
        let command = 
            match Entity.getNextTask entity' with
            | None -> AnnihilateEntity entityId
            | Some task -> EnterEntityInWaitlist(entityId, task.MachineId)
            |> fun action -> 
                { Time = time
                  Action = action }
        ({ state' with Time = time }, [ command ])

    | CapacityReleased(_, locationId, capacity) -> 
        let location = State.getLocation locationId state
        let location' = Location.releaseCapacity capacity location
        let state' = State.updateLocation location' state
        
        let command = 
            { Time = time
              Action = TrySelectEntityFromWaitlist locationId }
        ({ state' with Time = time }, [ command ])

    | EntityAnnihilated entityId -> 
        // TEMP: consistency check
        match State.getEntity entityId state |> Entity.getNextTask with
        | Some task -> 
            sprintf "Inconsistency => Entity still has a pending task: entity id = %A, task location id = %A" entityId 
                task.MachineId |> failwith
        | None -> ()
        let state' = State.deleteEntity entityId state
        ({ state' with Time = time }, [])

let private initModel jobShopData = 
    { InitialState = State.ofData jobShopData
      Execute = execute
      Apply = apply }

let private initSchedule jobShopData = 
    JobShopData.getAllJobs jobShopData
    |> Seq.map (fun job -> 
           { Time = job.ReleaseDate
             Action = CreateEntity job.Id })
    |> Schedule.ofSeq

let initSimulation jobShopData = Simulation.createInitial (initModel jobShopData) (initSchedule jobShopData)




(**

/// Represents the current state of the system and holds all locations. 
/// It is the aggregate root. The location ids are in the state context.
type State<'Time, 'Location> = 
    { Time : 'Time
      Locations : Repository<'Location Id, 'Location> }

module State = 
    
    /// Creates a new state without any locations.
    let create<'Location> time = 
        { Time = time
          Locations = Repository.createDefault<'Location> () }
    
    /// Adds a new location and returns the new id along with the updated state.
    /// The id is generated and passed to the factory function makeLocation to 
    /// make the location with that id. The id along with the updated state 
    /// is returned.
    /// Repository throws an exeption if the generated id already exists.
    let addLocation makeLocation state = 
        let (id, repo) = Repository.insert makeLocation state.Locations
        (id, { state with Locations = repo } )
    
    /// Gets the location with the given id and returns it in a Result.Success.
    /// If the id is not present a Result.Failure is returned.
    let getLocation id state = Repository.get id state.Locations

    /// Updates the location with the given id and returns the updated state
    /// in a Result.Success.
    /// If the id is not present a Result.Failure is returned.
    let updateLocation id location state =
        Repository.update id location state.Locations
        |> Result.mapR (fun repo -> { state with Locations = repo })



**)

(*
Execute: Can a command always be turned into events or can there be failures?
Apply: Can an event always be applied or can there be failures?
*)




// When an event happene
// When an event happened there are commands being issued!
//let CommandsFromEvent = 
//    function 
//    | Created(time, entity) -> 1
//    | EnteredWaitlist(time, entity, location) -> 2
//    | LeftWaitlist(time, entity, location) -> 3
//    | EnteredInputBuffer(time, entity, location) -> 4
//    | LeftInputBuffer(time, entity, location) -> 5
//    | EnteredOutputBuffer(time, entity, location) -> 6
//    | LeftOutputBuffer(time, entity, location) -> 7
//    | StartedProcess(time, entity, location) -> 8
//    | EndedProcess(time, entity, location) -> 9
(*
type Waitlist<'entity> = 
    { Validate : 'entity -> bool
      Compare : 'entity -> 'entity -> int
      Items : 'entity list }

module Waitlist = 
    let Create validate compare = 
        { Validate = validate
          Compare = compare
          Items = [] }

type Location<'Entity, 'Buffer, 'Waitlist> = 
    { Id : Location<'Entity, 'Buffer, 'Waitlist> Id
      Capacity : int
      Waitlist : 'Waitlist
      InputBuffer : 'Buffer
      OutputBuffer : 'Buffer }

    let Push : 'entity * 'entity Waitlist -> 'entity Waitlist
    let Pop : 'entity Waitlist -> ('entity * 'entity Waitlist) option
*)



(*
module Location = 
    let Create id capacity inputBufferCapacity ouputBufferCapacity =
        if capacity < 1 then
            None
        elif inputBufferCapacity < 0 then
            None
        elif ouputBufferCapacity < 0 then
            None
        else
            { Id = id
              Capacity = capacity
              Waitlist = Waitlist.Create
              InputBuffer = Buffer.Create inputBufferCapacity
              OutputBuffer = Buffer.Create ouputBufferCapacity }
            |> Some
*)
