#load "Common.fs"
#load "JobShopData.fs"

open System
open SimJobShop.Common
open SimJobShop.JobShopData


// ======================================
// Data Generation
// ======================================
#load "JobShopDataGeneration.fs"
open SimJobShop.JobShopDataGeneration

(*
Preis pro Maschinen-Stunde

Amortisationskosten von Maschinen!
Was ist wenn ich eine Maschine nur 1 mal brauchen?
Preis einer Maschine ist 
*)

let p = 
    { MachineCount = 10  // 20, 100
      MinTaskCount = 5   // 8, 40
      MaxTaskCount = 8   // 15, 75
      MinProcessingTime = TimeSpan.FromMinutes(20.0)
      MaxProcessingTime = TimeSpan.FromMinutes(60.0)
      MinCapacityNeeded = 1
      MaxCapacityNeeded = 1
      MinPrice = 1.0
      MaxPrice = 10000.0
      MinUnitsPerYear = 100
      MaxUnitsPerYear = 1000
      ProductCount = 20  // 120
      JobCount = 400//0
      }

let data = generateJobShopData 1 p
JobShopData.writeDataToFiles """C:\Temp\Test""" data


// ======================================
// Simulation
// ======================================
#load "Engine.fs"
#load "JobShopModelOutputBuffers.fs"
open SimJobShop.Engine
open SimJobShop.JobShopModelOutputBuffers


let mutable eventsLog = List.empty<Event>
let saveEvent event = eventsLog <- event::eventsLog
let log = ignore // printfn "%s"

let initial = initSimulation data


#time
let final = Simulation.run saveEvent log initial
#time

(*
100 jobs: Real: 00:00:08.954, CPU: 00:00:08.953, GC gen0: 84, gen1: 1, gen2: 0
200 jobs: Real: 00:00:17.960, CPU: 00:00:17.890, GC gen0: 167, gen1: 2, gen2: 0
300 jobs: Real: 00:00:25.547, CPU: 00:00:25.531, GC gen0: 251, gen1: 3, gen2: 0
400 jobs: Real: 00:00:35.700, CPU: 00:00:35.562, GC gen0: 337, gen1: 4, gen2: 0

derivative of time as a function of number of jobs
2 : 1 => 0.9991064448
3 : 1 => 0.9505566104
3 : 2 => 0.9514067449
4 : 1 => 0.9930190997
4 : 2 => 0.9939072107
4 : 3 => 1.044671184
*)

let r = eventsLog |> Seq.rev |> Event.writeEventsToFile "\t" """C:\Temp\Test\events.txt""" 
