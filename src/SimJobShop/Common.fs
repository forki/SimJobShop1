/// Some types and functions for common use in other modules.
/// This includes the Result type that is used for returning
/// values in cases where not always a valid result can be assured.
module SimJobShop.Common

// ======================================
// Common types
// ======================================

/// Id type
type 'entity Id = Id of uint64

/// Makes an Id generator function
let makeIdGenerator () =
    let mutable lastId = 0UL
    fun () ->
        lastId <- lastId + 1UL
        Id lastId

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
    
    // Infix version of map.
    let (<!>) = mapR
    // Apply a function in the result world.
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
