module SimJobShop.JobShopModel

type CommandAction<'Entity, 'Location> = 
    | Create of 'Entity
    | EnterWaitlist of 'Entity * 'Location
    | LeaveWaitlist of 'Location
    | MoveToInputBuffer of 'Entity * 'Location
    | MoveToOutputBuffer of 'Entity * 'Location
    | StartProcess of 'Entity * 'Location
    | EndProcess of 'Entity * 'Location

type Event<'Time, 'Fact> = 
    { Time : 'Time
      Fact : 'Fact }

type EventFact<'Time, 'Entity, 'Location> = 
    | Created of 'Time * 'Entity
    | EnteredWaitlist of 'Time * 'Entity * 'Location
    | LeftWaitlist of 'Time * 'Entity * 'Location
    | EnteredInputBuffer of 'Time * 'Entity * 'Location
    | LeftInputBuffer of 'Time * 'Entity * 'Location
    | EnteredOutputBuffer of 'Time * 'Entity * 'Location
    | LeftOutputBuffer of 'Time * 'Entity * 'Location
    | StartedProcess of 'Time * 'Entity * 'Location
    | EndedProcess of 'Time * 'Entity * 'Location

// When an event happened there are commands being issued!
let CommandsFromEvent = 
    function 
    | Created(time, entity) -> 1
    | EnteredWaitlist(time, entity, location) -> 2
    | LeftWaitlist(time, entity, location) -> 3
    | EnteredInputBuffer(time, entity, location) -> 4
    | LeftInputBuffer(time, entity, location) -> 5
    | EnteredOutputBuffer(time, entity, location) -> 6
    | LeftOutputBuffer(time, entity, location) -> 7
    | StartedProcess(time, entity, location) -> 8
    | EndedProcess(time, entity, location) -> 9

type Id<'a> = 
    | Id of uint64

type Location<'entity, 'buffer, 'waitlist> = 
    { Id : Location<'entity, 'buffer, 'waitlist> Id
      Capacity : int
      Waitlist : 'waitlist
      InputBuffer : 'buffer
      OutputBuffer : 'buffer }

/// A generic waitlist for entities that uses a comparer to rank the entities and 
type Waitlist<'entity> = 
    { Validator : 'entity -> bool
      Comparer : 'entity -> 'entity -> int
      Stack : 'entity list }

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
