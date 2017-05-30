/// Some types and functions for common use in other modules.
/// This includes the Result type that is used for returning
/// values in cases where not always a valid result can be assured.
module SimJobShop.Common

// ======================================
// Common types
// ======================================

/// Id type using a uint64 
type Id<'a> = Id of uint64

/// Functions to use with Id<'a>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Id = 
    /// Makes an Id generator function for the given type
    let makeGenerator<'a> () : (unit -> 'a Id) =
        let mutable lastId = 0UL
        fun () ->
            lastId <- lastId + 1UL
            Id lastId
    
    /// Returns a string of the numeric value of the id.
    let print (Id id) = sprintf "%i" id


// ======================================
// Units of measure
// ======================================
open Microsoft.FSharp.Data.UnitSystems.SI

/// Second (unit of measure)
[<Measure>]
type second = UnitNames.second

/// Minute in seconds (unit of measure)
let minute = 60.0<second>

/// Hour in seconds (unit of measure)
let hour = 3600.0<second>

/// Hour in seconds (unit of measure)
let day = 86400.0<second>


/// Meter (unit of measure)
[<Measure>]
type meter = UnitNames.metre

/// Centimeter in meter (unit of measure)
let centimeter = 0.01<meter>

/// Millimeter in meter (unit of measure)
let milimeter = 0.001<meter>



// ======================================
// Loggers
// ======================================

let logToConsole msg = printfn "%s" msg

let logIgnore (msg:string) = ignore msg



// ======================================
// Result type
// ======================================
/// Represents a result that allows for passing values 
/// in case of success or error messages in case of failure.
type Result<'a, 'error> = 
    | Success of 'a
    | Failure of 'error

/// Functions to work with Result<'a, 'error>
module Result = 
    /// Return a success, i.e. lift into the result world.
    let returnR x = Success x
    
    /// Bind a result yielding function to a result input.
    let bindR f xR = 
        match xR with
        | Success x -> f x
        | Failure err -> Failure err
    
    /// Infix version of bind.
    let (>>=) xR f = bindR f xR
    
    /// Map: Lift a one-parameter function to result world.
    let mapR f = bindR (f >> returnR)
    
    /// Infix version of map.
    let (<!>) = mapR

    /// Apply a function in the result world.
    let applyR fR xR = fR >>= (fun f -> xR >>= (fun x -> returnR (f x)))
    
    /// Infix version of apply.
    let (<*>) = applyR
    
    /// Lift a one-parameter function to result world (same as mapR).
    let lift1R f x = f <!> x
    
    /// Lift a two-parameter function to result world.
    let lift2R f x y = f <!> x <*> y
    
    /// Computation Expression
    type ResultBuilder() = 
        member this.Bind(m : Result<'a, 'error>, f : 'a -> Result<'b, 'error>) = bindR f m
        member this.Return(x) : Result<'a, 'error> = returnR x
        member this.ReturnFrom(m) : Result<'a, 'error> = m
        member this.Zero() : Result<unit, 'error> = this.Return()
        member this.Combine(m1, f) = this.Bind(m1, f)
        member this.Delay(f) = f
        member this.Run(m) = m()
        
        member this.TryWith(m : Result<'a, 'error>, h : exn -> Result<'a, 'error>) = 
            try 
                this.ReturnFrom(m)
            with e -> h e
        
        member this.TryFinally(m : Result<'a, 'error>, compensation) = 
            try 
                this.ReturnFrom(m)
            finally
                compensation()
        
        member this.Using(res : #System.IDisposable, body) : Result<'b, 'error> = 
            this.TryFinally(body res, 
                            (fun () -> 
                            match res with
                            | null -> ()
                            | disp -> disp.Dispose()))
        
        member this.While(cond, m) = 
            if not (cond()) then this.Zero()
            else this.Bind(m(), fun _ -> this.While(cond, m))
        
        member this.For(sequence : seq<_>, body) = 
            this.Using(sequence.GetEnumerator(), (fun enum -> this.While(enum.MoveNext, fun _ -> body enum.Current)))
    
    let result = ResultBuilder()

    /// Transform an option into a result given an error message for None.
    let ofOption error = function
        | Some x -> Success x
        | None -> Failure error
    
    /// Transform a boolean into a result given a success object for True and an error message for False.
    let ofBool success error = function
        | true -> Success success
        | false -> Failure error

    /// Return value on success or failwith error on failure.
    let getValue xR =
        match xR with
        | Success x -> x
        | Failure e -> failwith e


// ======================================
// Repository type
// ======================================
/// Represents an generic, immutable repository of items organised by id.
/// A function NewId must be provided that generates unique Ids.
type Repository<'id, 'item when 'id : comparison> = 
    { Items : Map<'id, 'item>
      NewId : unit -> 'id }

module Repository = 
    /// Returns true if the repository already contains the given id.
    let containsId id repo =
        Map.containsKey id repo.Items
    
    /// Adds an item with given id and returns a Result.Success with the repository.
    /// A Result.Failure is returned if the repository already containts that id.
    let private add id item repo = 
        if containsId id repo then
            sprintf "Repository.add: Id already exists, id = %A" id
            |> Failure
        else
            { repo with Items = Map.add id item repo.Items }
            |> Success
    
    /// Gets the item with the given id from the repository and returns it in a 
    /// Result.Success. If the id is not present a Result.Failure is returned.
    let get id repo = 
        Map.tryFind id repo.Items
        |> Result.ofOption (sprintf "Repository.get: Item not found, id = %A" id)
    
    /// Inserts a new item into the repository and returns the new id along with the 
    /// updated repository. The id is generated and passed to the factory function 
    /// makeItem to make the item with that id. The id along with the updated 
    /// repository is returned. Throws an exeption if the generated id already exists.
    let insert makeItem repo = 
        let id = repo.NewId()
        let item = makeItem id
        match add id item repo with
        | Success newRepo -> (id, newRepo)
        | Failure _ -> sprintf "Repository.insert: Id exists already, id = %A" id |> failwith
    
    /// Deletes the item with the given id from the repository and returns the 
    /// updated repository in a Result.Success. If the id is not present a 
    /// Result.Failure is returned. 
    let delete id repo = 
        if containsId id repo then
            { repo with Items = Map.remove id repo.Items }
            |> Success
        else
            sprintf "Repository.delete: Item not found, id = %A" id
            |> Failure

    /// Updates the item with the given id from the repository and returns the 
    /// updated repository in a Result.Success. If the id is not present a 
    /// Result.Failure is returned. 
    let update id item repo =
        delete id repo
        |> Result.bindR (fun r -> add id item r)
    
    /// Transforms the item with the given id using the transformation f and returns the 
    /// updated repository in a Result.Success. If the id is not present a 
    /// Result.Failure is returned. 
    let transform id f repo =
        get id repo
        |> Result.mapR f
        |> Result.bindR (fun item -> update id item repo)

    /// Returns a sequence of all Ids in the repository.
    let getAllIds repo =
        repo.Items |> Map.toSeq |> Seq.map fst

    /// Returns a sequence of all Ids in the repository.
    let getAllItems repo =
        repo.Items |> Map.toSeq |> Seq.map snd
    
    
    /// Creates a new (empty) repository with the given id generating function.
    let create<'id, 'item when 'id : comparison> idGenerator = 
        { Items = Map.empty<'id, 'item>
          NewId = idGenerator }
    
    /// Creates a new (empty) repository using the Id<'Item> type.
    let createDefault<'item>() =
        Id.makeGenerator<'item>()
        |> create<'item Id, 'item>


// ======================================
// Random data generation
// ======================================
module Random = 
    open System
    
    let makeGenerator seed = Random(seed)
    let bool (rnd : Random) probOfTrue = rnd.NextDouble() < probOfTrue
    let int (rnd : Random) min max = rnd.Next(min, max)
    let uniform (rnd : Random) min max = (max - min) * rnd.NextDouble() + min
    
    let normalSequenceInfinite (rnd : Random) mean sigma = 
        // Number of samples to average [4 to 10?] (tails stretch/flatten out as this gets larger)
        let nSamples = 10
        // calc normalization factors up front & alloc a random()   
        let norm = sigma * sqrt (3 * nSamples |> float)
        let shift = norm - mean
        let scale = 2.0 * norm / (float nSamples)
        
        // return a gaussian # by averaging another random seq (central limit theory)
        let rec gaussAvg n acc = 
            if n > 0 then gaussAvg (n - 1) (acc + rnd.NextDouble())
            else acc * scale - shift
        
        let rec gaussSeq() = 
            seq { 
                yield gaussAvg nSamples 0.0
                yield! gaussSeq()
            }
        
        gaussSeq()
    
    let boolSequenceInfinite (rnd : Random) probOfTrue = Seq.initInfinite (fun _ -> bool rnd probOfTrue)
    let boolSequence (rnd : Random) probOfTrue count = Seq.init count (fun _ -> bool rnd probOfTrue)
    let intSequenceInfinite (rnd : Random) min max = Seq.initInfinite (fun _ -> int rnd min max)
    let intSequence (rnd : Random) min max count = Seq.init count (fun _ -> int rnd min max)
    let uniformSequenceInfinite (rnd : Random) min max = Seq.initInfinite (fun _ -> uniform rnd min max)
    let uniformSequence (rnd : Random) min max count = Seq.init count (fun _ -> uniform rnd min max)
    let filter (rnd : Random) probOfTrue array = array |> Array.filter (fun _ -> bool rnd probOfTrue)
    let shuffle (rnd : Random) array = array |> Array.sortBy (fun _ -> rnd.NextDouble())
    
    let sampleNoReplace (rnd : Random) count array = 
        array
        |> shuffle rnd
        |> Array.take count
    
    let sampleReplace (rnd : Random) count array = 
        let n = Array.length array
        intSequence rnd 0 (n - 1) count
        |> Seq.toArray
        |> Array.map (fun i -> array.[i])
