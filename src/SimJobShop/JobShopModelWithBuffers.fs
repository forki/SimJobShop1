module SimJobShop.JobShopModelWithBuffers

(* 
UNFINISHED!

This model is an attempt to incorporate finite capacity input/output buffers
for the locations.
*)


open System
//#load "Common.fs"
open SimJobShop.Common
//#load "Engine.fs"
open SimJobShop.Engine

//open Common
//open Engine
type EntityId = int

type LocationId = int

type Buffer<'a> = 
    { Items : 'a list
      CapacityTotal : Capacity
      CapacityAvailable : Capacity }

type Location = 
    { Id : LocationId
      Waitlist : EntityId list
      InputBuffer : Buffer<EntityId>
      // OutputBuffer : Buffer<EntityId>
      Processing : Buffer<EntityId> }

type Task = 
    { LocationId : LocationId
      // CapacityNeeded : Capacity
      ProcessingTime : TimeSpan }

type Entity = 
    { Id : EntityId
      PendingTasks : Task list
      CurrentTask : Task option }

type State = 
    { Time : DateTime
      // Data : JobShopData
      Entities : Map<EntityId, Entity>
      Locations : Map<LocationId, Location> }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Entity = 
    let capacity = FiniteCapacity 1u
    let create tasks id = 
        { Id = id
          PendingTasks = tasks
          CurrentTask = None }
    
    let tryGetNextTask entity =
        List.tryHead entity.PendingTasks

    let tryGetNextLocationId entity =
        tryGetNextTask entity
        |> Option.map (fun task -> task.LocationId)

    let tryGetCurrentLocationId entity =
        entity.CurrentTask
        |> Option.map (fun task -> task.LocationId)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Buffer = 
    let empty<'a> capTotal = 
        { Items = List.empty<'a>
          CapacityTotal = capTotal
          CapacityAvailable = Capacity.zero }
    
    let hasCapacity capacity buffer = Capacity.isGreaterOrEqual buffer.CapacityAvailable capacity
    
    let tryAdd (item, capacity) buffer = 
        if hasCapacity capacity buffer then 
            { buffer with Items = item :: buffer.Items
                          CapacityAvailable = Capacity.subtract buffer.CapacityAvailable capacity }
            |> Some
        else None
    
    let remove (item, capacity) buffer = 
        let items = List.filter (fun i -> i <> item) buffer.Items
        let cap = 
            if buffer.Items.Length = items.Length then buffer.CapacityAvailable
            else Capacity.add buffer.CapacityAvailable capacity
        { buffer with Items = items
                      CapacityAvailable = cap }



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Location = 
    let create (inputBufferCap, processingCap) id = 
        if Capacity.isZero processingCap then 
            sprintf "Location Id=%A cannot have zero ProcessingCapacity" id |> failwith
        { Id = id
          Waitlist = List.empty<EntityId>
          InputBuffer = Buffer.empty<EntityId> inputBufferCap
          Processing = Buffer.empty<EntityId> processingCap }
    
    let enterInWaitlist entityId location =
        { location with
            Waitlist = entityId :: location.Waitlist |> List.distinct }
    
    let removeEntityFromWaitlist entityId location = 
        { location with
            Waitlist = List.filter (fun id -> id <> entityId) location.Waitlist }

    let canEnterInputBuffer capacity location =
        Buffer.hasCapacity capacity location.InputBuffer
    
    let trySelectEntityFromWaitlist location = 
        if canEnterInputBuffer Entity.capacity location then List.tryLast location.Waitlist
        else None

    let tryEnterInputBuffer entityId location = 
        Buffer.tryAdd (entityId, Entity.capacity) location.InputBuffer 
        |> Option.map (fun buffer -> { location with InputBuffer = buffer })

    let revomeEntityFromProcessing entityId location =
        { location with
            Processing = Buffer.remove (entityId, Entity.capacity) location.Processing }
        




[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State = 
    let createEntity tasks id state = 
        if Map.containsKey id state.Entities then sprintf "Entity id=%A already exists" id |> failwith
        let entity = Entity.create tasks id
        entity, { state with Entities = Map.add id entity state.Entities }
    
    let createLocation (inputBufferCap, processingCap) id state = 
        if Map.containsKey id state.Locations then sprintf "Location Id=%A already exists" id |> failwith
        let location = Location.create (inputBufferCap, processingCap) id
        { state with Locations = Map.add id location state.Locations }
    
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

    let replaceLocation (location : Location) state = 
        { state with Locations = Map.add location.Id location state.Locations }

    let transformLocation locationId transform state =
        getLocation locationId state
        |> transform
        |> fun location -> replaceLocation location state

    let tryTransformLocation locationId tryTransform state =
        getLocation locationId state
        |> tryTransform
        |> Option.map (fun location -> replaceLocation location state)

    let trySelectEntityFromWaitlist locationId state =
        getLocation locationId state
        |> Location.trySelectEntityFromWaitlist



type CommandAction = 
    | CreateEntity of EntityId * Task list
    | EnterEntityInWaitlist of EntityId * LocationId
    | TrySelectEntityFromWaitlist of LocationId
    | ReleaseCurrentLocationCapacity of EntityId
    | TryTakeFromInputBuffer of EntityId * LocationId
    | AnnihilateEntity of EntityId

type EventFact = 
    | EntityCreated of EntityId * Task list
    | EntityEnteredInWaitlist of EntityId * LocationId
    | EntitySelectedFromWaitlist of EntityId * LocationId
    | EntityEnteredInputBuffer of EntityId * LocationId
    | CurrentLocationCapacityReleased of EntityId * LocationId
    | LocationCapacityBlocked of EntityId * LocationId * Capacity
    | EntityEnteredLocation of EntityId * LocationId
    | ChangeoverStarted of EntityId * LocationId
    | ChangeoverEnded of EntityId * LocationId
    | ProcessingStarted of EntityId * LocationId
    | ProcessingEnded of EntityId * LocationId
    | EntityAnnihilated of EntityId

type Time = DateTime

type Command = 
    { Time : DateTime
      Action : CommandAction }

type Event = 
    { Time : DateTime
      Fact : EventFact }


let execute (state : State) (command : Command) : Event list = 
    let time = command.Time
    match command.Action with

    | CreateEntity (entityId, tasks) -> 
        [ { Time = time; Fact = EntityCreated (entityId, tasks) } ]

    | EnterEntityInWaitlist (entityId, locationId) -> 
        [ { Time = time; Fact = EntityEnteredInWaitlist (entityId, locationId) } ]

    | TrySelectEntityFromWaitlist locationId ->
        State.trySelectEntityFromWaitlist locationId state
        |> Option.toList
        |> List.collect (fun entityId ->
            [ EntitySelectedFromWaitlist (entityId, locationId)
              EntityEnteredInputBuffer (entityId, locationId)
            ])
        |> List.map (fun fact -> {Time = time; Fact = fact })

    | ReleaseCurrentLocationCapacity entityId ->
        let transform = Location.revomeEntityFromProcessing entityId
        State.getEntity entityId state
        |> Entity.tryGetCurrentLocationId
        |> Option.map (fun locationId -> CurrentLocationCapacityReleased (entityId, locationId))
        |> Option.map (fun fact -> { Time = time; Fact = fact })
        |> Option.toList

    | TryTakeFromInputBuffer (entityId, locationId) -> 
        //TODO


let apply (state : State) (event : Event) : State * Command list = 
    let time = event.Time
    match event.Fact with

    | EntityCreated (entityId, tasks) -> 
        let entity, state' = State.createEntity tasks entityId state
        let action = 
            match Entity.tryGetNextLocationId entity with
            | Some locationId -> EnterEntityInWaitlist(entityId, locationId)
            | None -> AnnihilateEntity entityId
        state', [ { Time = time; Action = action } ]

    | EntityEnteredInWaitlist (entityId, locationId) ->
        let transform = Location.enterInWaitlist entityId
        let state' = State.transformLocation locationId transform state
        state', [ { Time = time; Action = TrySelectEntityFromWaitlist locationId } ]
    
    | EntitySelectedFromWaitlist (entityId, locationId) ->
        let transform = Location.removeEntityFromWaitlist entityId
        let state' = State.transformLocation locationId transform state
        state', []

    | EntityEnteredInputBuffer (entityId, locationId) ->
        let tryTransform = Location.tryEnterInputBuffer entityId
        let state' = 
            match State.tryTransformLocation locationId tryTransform state with
            | None -> sprintf "Inconsistency => Cannot enter Input Buffer: entity id = %A, location id = %A" entityId locationId |> failwith
            | Some state' -> state'  //TODO: TryTakeFromInputBuffer
        let commands =
            [ ReleaseCurrentLocationCapacity entityId
              TryTakeFromInputBuffer (entityId, locationId) ]
        state', []


    | CurrentLocationCapacityReleased (entityId, locationId) ->
        //TODO
