#r @"..\..\packages\ExcelProvider\lib\ExcelProvider.dll"
#load "Common.fs"
#load "JobShopData.fs"
#load "FlexibleJobShopData.fs"
#load "FlexibleJobShopDataGeneration.fs"

open System
open SimJobShop.Common
open SimJobShop.JobShopData
open SimJobShop.FlexibleJobShopData
open SimJobShop.FlexibleJobShopDataGeneration

let OUTPUTDIRECTORY = """C:\Temp\Complexity"""

// ======================================
// Data Generation
// ======================================

let p = 
    { Phases = [1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5; 5; 5; 5]  // 20 Stages devided into 5 phases
      MachinesPerStage = 2
      MinTaskCount = 5   // 8, 40
      MaxTaskCount = 8   // 15, 75
      MinProcessingTime = TimeSpan.FromMinutes(5.0)
      MaxProcessingTime = TimeSpan.FromMinutes(10.0)
      MinCapacityNeeded = 1
      MaxCapacityNeeded = 1
      MinPrice = 1.0
      MaxPrice = 10000.0
      MinUnitsPerYear = 100
      MaxUnitsPerYear = 1000
      ProductCount = 20  // 120
      JobCount = 1000
      ThreasholdUnitsPerYear = 50
      SlowMachinesMinSpeedFactor = 0.75
      SlowMachinesMaxSpeedFactor = 0.9 }

//let flexData = generateJobShopData 1 p
let flexData = generateJobShopDataFromRealData 42 p
FlexibleJobShopData.writeDataToFiles OUTPUTDIRECTORY flexData





// ======================================
// Scheduling
// transform FlexibleJobShopData -> JobShopData
// ======================================


let data0 = JobShopData.create()

// Map machines
let (machineMap, data1) =
    flexData
    |> FlexibleJobShopData.getAllMachines
    |> Seq.fold (fun (map, data) flexMachine -> 
        JobShopData.makeMachine (flexMachine.Capacity, flexMachine.InputBufferCapacity) data
        |> fun (machineId, data) -> 
            (Map.add flexMachine.Id machineId map, data)
        ) (Map.empty, data0)

// Map products
let (productMap, data2) =
    flexData
    |> FlexibleJobShopData.getAllProducts
    |> Seq.fold (fun (map, data) flexProduct -> 
        JobShopData.makeProduct (flexProduct.Price, flexProduct.Cost, flexProduct.UnitsPerYear) data
        |> fun (productId, data) -> 
            (Map.add flexProduct.Id productId map, data)
        ) (Map.empty, data1)


// Function to allocate the flexible tasks to machines
let allocateTask rnd flexData flexTask =
    flexData
    |> FlexibleJobShopData.getAllMachines
    |> Seq.filter (fun flexMachine -> flexMachine.StageId = flexTask.StageId)
    |> Seq.toArray
    |> Random.sampleOne rnd
    |> fun flexMachine -> flexMachine.Id



let rnd = Random.makeGenerator 42
let allocate = allocateTask rnd flexData

// Map jobs
let (jobMap, data) =
    flexData
    |> FlexibleJobShopData.getAllJobs
    |> Seq.fold (fun (map, data) flexJob -> 
        let tasklist = 
            flexData
            |> FlexibleJobShopData.getProduct flexJob.ProductId
            |> Result.mapR (fun flexProduct -> 
                flexProduct.FlexibleTasks
                |> List.map (fun flexTask -> 
                    let flexMachineId = flexTask |> allocate
                    let machineId = Map.find flexMachineId machineMap
                    let processingTime = 
                        FlexibleJobShopData.getMachine flexMachineId flexData
                        |> Result.mapR (fun flexMachine -> 
                            flexTask.BaseProcessingTime.TotalMinutes
                            |> (*) (1.0 / flexMachine.SpeedFactor)
                            |> TimeSpan.FromMinutes
                            )
                        |> Result.getValue
                    JobShopData.makeTask (machineId, flexTask.Rank, processingTime, flexTask.CapacityNeeded) data1
                    )
                )
            |> Result.getValue
        let productId = Map.find flexJob.ProductId productMap
        JobShopData.makeJob (productId, tasklist, flexJob.ReleaseDate, flexJob.DueDate) data
        |> fun (jobId, data) -> 
            (Map.add flexJob.Id jobId map, data)
        ) (Map.empty, data2)


JobShopData.writeDataToFiles (OUTPUTDIRECTORY + """\ScheduledRandom""") data





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

let r = eventsLog |> Seq.rev |> Event.writeEventsToFile "\t" (OUTPUTDIRECTORY + """\ScheduledRandom\events.txt""")


