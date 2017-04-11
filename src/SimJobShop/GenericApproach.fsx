type CommandAction<'entity, 'location> = 
    | Create of 'entity
    | MoveToWaitlist of 'entity * 'location
    | MoveToInputBuffer of 'entity * 'location
    | MoveToOutputBuffer of 'entity * 'location
    | StartProcess of 'entity * 'location
    | EndProcess of 'entity * 'location
    | TakeFromWaitlist of 'location

type EventFact<'time, 'entity, 'location> = 
    | Created of 'time * 'entity
    | EnteredWaitlist of 'time * 'entity * 'location
    | LeftWaitlist of 'time * 'entity * 'location
    | EnteredInputBuffer of 'time * 'entity * 'location
    | LeftInputBuffer of 'time * 'entity * 'location
    | EnteredOutputBuffer of 'time * 'entity * 'location
    | LeftOutputBuffer of 'time * 'entity * 'location
    | StartedProcess of 'time * 'entity * 'location
    | EndedProcess of 'time * 'entity * 'location

type Event<'time, 'entity, 'location> = 
    { Time : 'time
      (** TODO: Do we need this too?
     ModelId : Model Id 
**)
      EventFact : EventFact<'time, 'entity, 'location> }

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
