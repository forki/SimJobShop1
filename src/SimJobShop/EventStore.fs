/// Event store implementation.
module SimJobShop.EventStore

/// Implements a basic in memory event store using a dictionary.
type InMemoryEventStore<'Id when 'Id : equality>() = 
    
    /// Private mutable storage data.
    let eventDict = System.Collections.Generic.Dictionary<'Id, obj list>()
    
    /// Save an object (event) to storage.
    member this.Save(id, event) = 
        match eventDict.TryGetValue id with
        | true, eventList -> 
            // store newest in front
            eventDict.[id] <- event :: eventList
        | false, _ -> 
            // store first event as single item list
            eventDict.[id] <- [ event ]
    
    /// Get all events for eventId in reversed chronological order (newest first).
    member this.GetReversed<'Event> eventId = 
        match eventDict.TryGetValue eventId with
        | true, eventList -> 
            eventList
            |> Seq.cast<'Event>
            |> Seq.toList
        | false, _ -> []
    
    /// Get all events for eventId in chronological order (oldest first).
    member this.Get<'Event> eventId = this.GetReversed<'Event> eventId |> List.rev
    
    /// Clear all events associated with the specified eventId.
    member this.Clear eventId = eventDict.[eventId] <- []
