module SimJobShop.JobShopModel

open System
open Common
open Engine
open JobShopData


type EntityId = Job Id
type Entity = {JobId : Job Id; PendingTasks : Task list}
type LocationId = Machine Id
type Location = {MachineId : Machine Id; CapacityAvailable : Capacity; CapacityTotal : Capacity; Waitlist : EntityId list}
type State = {
    Time : DateTime
    Data : JobShopData
    Entities : Map<EntityId, Entity>
    Locations : Map<LocationId, Location>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Entity =
    let getNextTask entity =
        match entity.PendingTasks with
        | [] -> None
        | t :: _ -> Some t
    
    let getNextMachineId entity =
        getNextTask entity
        |> Option.map (fun task -> task.MachineId)
        
    let getNextCapacityNeeded entity =
        getNextTask entity
        |> Option.map (fun task -> task.CapacityNeeded)

    let removeTask task entity =
        entity.PendingTasks
        |> List.filter (fun t -> t <> task)
        |> fun tasklist -> { entity with PendingTasks = tasklist }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Location =

    /// Select the next entity from the waitlist.
    /// TEMP: Strictly FIFO!
    let selectEntityFromWaitlist location =
        List.tryLast location.Waitlist

    let removeEntityFromWaitlist entityId location =
        location.Waitlist
        |> List.filter (fun id -> id <> entityId)
        |> fun waitlist -> { location with Waitlist = waitlist }

    let isCapacityAvailable location entity =
        Entity.getNextCapacityNeeded entity
        |> Option.map (Capacity.isGreaterOrEqual location.CapacityAvailable)
        |> function | None -> false | Some b -> b

    /// Add the entity as head of the waitlist.
    let addToWaitlist entityId location =
        { location with Waitlist = entityId :: location.Waitlist }
        
    let blockCapacity capacity location =
        if Capacity.isGreater capacity location.CapacityAvailable then
            sprintf "Inconsistency => capacity to be blocked is larger than available capacity: location id = %A" location.MachineId
            |> failwith
        { location with CapacityAvailable = Capacity.subtract location.CapacityAvailable capacity }
    
    /// Release the given capacity at this location.
    /// Throws execption if resulting capacity is larger than total capacity.
    let releaseCapacity capacity location =
        match Capacity.add location.CapacityAvailable capacity with
        | c when Capacity.isGreater c location.CapacityTotal ->
            sprintf "Inconsistency => resulting capacity is larger than total capacity: location id = %A" location.MachineId
            |> failwith
        | c -> 
            { location with CapacityAvailable = c }


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
    let createLocation (machine:Machine) state =
        { MachineId = machine.Id; CapacityAvailable = machine.Capacity; CapacityTotal = machine.Capacity; Waitlist = [] }
        |> addLocation state

    let ofData jobShopData =
        let time = JobShopData.getEarliestReleaseDate jobShopData
        let state0 = create time jobShopData
        JobShopData.getAllMachines jobShopData
        |> Seq.fold (fun state machine -> createLocation machine state) state0

    /// Create a new entity based on a given job.
    /// This assumes that job.Id is unique!
    let createEntity (jobId:Job Id) (state:State) =
        let jobResult = JobShopData.getJob jobId state.Data
        let productResult = 
            jobResult
            |> Result.bindR (fun job -> JobShopData.getProduct job.ProductId state.Data)
        Result.lift2R (fun job product -> { JobId = job.Id; PendingTasks = product.Tasks }) jobResult productResult
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
    
    let updateLocation location state =
        let id = location.MachineId
        if not (Map.containsKey id state.Locations) then
            sprintf "Inconsistency => Location not found: location id = %A" id
            |> failwith
        { state with Locations = Map.add id location state.Locations }
    
    let updateEntity entity state =
        let id = entity.JobId
        if not (Map.containsKey id state.Entities) then
            sprintf "Inconsistency => Entity not found: entity id = %A" id
            |> failwith
        { state with Entities = Map.add id entity state.Entities }

    let deleteEntity entityId state =
        if not (Map.containsKey entityId state.Entities) then
            sprintf "Inconsistency => Entity not found: entity id = %A" id
            |> failwith
        { state with Entities = Map.remove entityId state.Entities }



type CommandAction = 
    | CreateEntityForJob of Job Id
    | EnterWaitlist of EntityId * LocationId
    | TrySelectEntityFromWaitlist of LocationId
//    | MoveToInputBuffer of EntityId * LocationId
//    | MoveToOutputBuffer of EntityId * LocationId
    | EndProcess of EntityId * LocationId
    | AnnihilateEntity of EntityId

type EventFact = 
    // An entity with this Id was created and moved to the source
    | CreatedEntityForJob of Job Id
    // The entity was entered in the waitlist of that location
    | EnteredWaitlist of EntityId * LocationId
    // The entity was selected and removed from the waitlist of that location
    | SelectedEntityFromWaitlist of EntityId * LocationId
//    | EnteredInputBuffer of EntityId * LocationId  // 
//    | LeftInputBuffer of EntityId * LocationId  // EntityId removed from output buffer of current location and command to enter the input buffer of the next location --> need current location
//    | EnteredOutputBuffer of EntityId * LocationId
//    | LeftOutputBuffer of EntityId * LocationId
    | StartedProcess of EntityId * LocationId
    | EndedProcess of EntityId * LocationId
    | AnnihilatedEntity of EntityId

type Time = DateTime

type Event = { Time : Time; Fact : EventFact }

type Command = Command<Time, CommandAction>


let execute state command =
    match command.Action with
    | CreateEntityForJob entityData ->
        [ { Time = command.Time; Fact = CreatedEntityForJob entityData } ]

    | EnterWaitlist (entityId, locationId) ->
        [ { Time = command.Time; Fact = EnteredWaitlist (entityId, locationId) } ]

    | TrySelectEntityFromWaitlist locationId -> 
        let location = State.getLocation locationId state
        Location.selectEntityFromWaitlist location
        |> Option.map (fun entityId -> State.getEntity entityId state)
        |> function
            | Some entity when Location.isCapacityAvailable location entity ->
                [ { Time = command.Time; Fact = SelectedEntityFromWaitlist (entity.JobId, locationId) }
                  { Time = command.Time; Fact = StartedProcess (entity.JobId, locationId) } ]
            | None | Some _ -> []
    
//    | MoveToInputBuffer (entityId, locationId) -> EnteredInputBuffer (entityId, locationId)
    
//    | MoveToOutputBuffer (entityId, locationId) -> EnteredOutputBuffer (entityId, locationId)

    | EndProcess (entityId, locationId) ->
        [ { Time = command.Time; Fact = EndedProcess (entityId, locationId) } ]

    | AnnihilateEntity entityId ->
        [ { Time = command.Time; Fact = AnnihilatedEntity entityId } ]

    
let apply state event =
    match event.Fact with
    | CreatedEntityForJob jobId -> 
        let entity, state' = State.createEntity jobId state
        let command =
            Entity.getNextMachineId entity
            |> function
                | Some locationId -> EnterWaitlist (entity.JobId, locationId)
                | None -> AnnihilateEntity entity.JobId
            |> fun action -> { Time = event.Time; Action = action }
        ({state' with Time = event.Time}, [command])

    | EnteredWaitlist (entityId, locationId) ->
        let location = State.getLocation locationId state
        let location' = Location.addToWaitlist entityId location
        let state' = State.updateLocation location' state
        let command = { Time = event.Time; Action = TrySelectEntityFromWaitlist locationId }
        ({state' with Time = event.Time}, [command])

    | SelectedEntityFromWaitlist (entityId, locationId) ->
        let location = State.getLocation locationId state
        let location' = Location.removeEntityFromWaitlist entityId location
        let state' = State.updateLocation location' state
        ({state' with Time = event.Time}, [])

    | StartedProcess (entityId, locationId) ->
        let location = State.getLocation locationId state
        let entity = State.getEntity entityId state
        let task =
            match Entity.getNextTask entity with
            | None ->
                sprintf "Inconsistency => Entity has no task left: entity id = %A, location id = %A" entityId locationId
                |> failwith
            | Some t -> t

        // TEMP: consistency checks
        if Capacity.isLess location.CapacityAvailable task.CapacityNeeded then
            sprintf "Inconsistency => Not enough capacity available: location id = %A, capacity available = %A, entity id = %A, capacity needed = %A" locationId location.CapacityAvailable entityId task.CapacityNeeded
            |> failwith
        if task.MachineId <> location.MachineId then
            sprintf "Inconsistency => wrong MachineId: expected = %A, actual = %A" id location.MachineId
            |> failwith

        let location' = Location.blockCapacity task.CapacityNeeded location
        let state' = State.updateLocation location' state
        let command = { Time = event.Time.Add task.ProcessingTime; Action = EndProcess (entityId, locationId) }
        ({state' with Time = event.Time}, [command])

    | EndedProcess (entityId, locationId) ->
        let location = State.getLocation locationId state
        let entity = State.getEntity entityId state
        
        let currentTask =
            match Entity.getNextTask entity with
            | None ->
                sprintf "Inconsistency => Entity has no current task: entity id = %A, location id = %A" entityId locationId
                |> failwith
            | Some task -> task

        let entity' = Entity.removeTask currentTask entity
        let location' = Location.releaseCapacity currentTask.CapacityNeeded location
        let state' =
            state
            |> State.updateLocation location'
            |> State.updateEntity entity'

        let locationCommand = { Time = event.Time; Action = TrySelectEntityFromWaitlist locationId }
        let entityCommand =
            match Entity.getNextTask entity' with
            | None -> AnnihilateEntity entityId
            | Some task -> EnterWaitlist (entityId, task.MachineId)
            |> fun action -> { Time = event.Time; Action = action }
        ({state' with Time = event.Time}, [entityCommand; locationCommand])

    | AnnihilatedEntity entityId ->
        // TEMP: consistency check
        match State.getEntity entityId state |> Entity.getNextTask with
            | Some task ->
                sprintf "Inconsistency => Entity still has a pending task: entity id = %A, task location id = %A" entityId task.MachineId
                |> failwith
            | None -> ()

        let state' = State.deleteEntity entityId state
        ({state' with Time = event.Time}, [])


let private initModel jobShopData =
    { InitialState = State.ofData jobShopData
      Execute = execute
      Apply = apply }

let private initSchedule jobShopData =
    JobShopData.getAllJobs jobShopData
    |> Seq.map (fun job -> { Time = job.ReleaseDate; Action = CreateEntityForJob job.Id } )
    |> Schedule.ofSeq

let initSimulation jobShopData =
    Simulation.createInitial (initModel jobShopData) (initSchedule jobShopData)





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
