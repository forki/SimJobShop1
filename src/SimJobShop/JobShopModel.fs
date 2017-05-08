module SimJobShop.JobShopModel

open System
open Common

(*
command + Actions
event + Facts
state
the model
*)

type CommandAction<'Entity, 'Location> = 
    | Create of 'Entity
    | EnterWaitlist of 'Entity * 'Location
    | LeaveWaitlist of 'Location * 'Location
    | MoveToInputBuffer of 'Entity * 'Location
    | MoveToOutputBuffer of 'Entity * 'Location
    | StartProcess of 'Entity * 'Location
    | EndProcess of 'Entity * 'Location

type Event<'Time, 'Fact> = 
    { Time : 'Time
      Fact : 'Fact }

type EventFact<'Entity, 'Location> = 
    | Created of 'Entity
    | EnteredWaitlist of 'Entity * 'Location
    | LeftWaitlist of 'Entity * 'Location
    | EnteredInputBuffer of 'Entity * 'Location
    | LeftInputBuffer of 'Entity * 'Location
    | EnteredOutputBuffer of 'Entity * 'Location
    | LeftOutputBuffer of 'Entity * 'Location
    | StartedProcess of 'Entity * 'Location
    | EndedProcess of 'Entity * 'Location

//type Id<'a> =  | Id of uint64 with
//    static member zero = Id 0UL 


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


type Location<'Entity, 'Buffer, 'Waitlist> = 
    { Id : Location<'Entity, 'Buffer, 'Waitlist> Id
      Capacity : int
      Waitlist : 'Waitlist
      InputBuffer : 'Buffer
      OutputBuffer : 'Buffer }

/// A generic waitlist for entities that uses a comparer to rank the entities and 
type Waitlist<'Entity> = 
    { Validator : 'Entity -> bool
      Comparer : 'Entity -> 'Entity -> int
      Stack : 'Entity list }

module Waitlist = 
    let Create validate compare = 
        { Validator = validate
          Comparer = compare
          Stack = [] }
(**
    let Push : 'entity * 'entity Waitlist -> 'entity Waitlist
    let Pop : 'entity Waitlist -> ('entity * 'entity Waitlist) option
**)



(**
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
**)
