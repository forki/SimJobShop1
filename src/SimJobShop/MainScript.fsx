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
      MinProcessingTime = TimeSpan.FromMinutes(20.0)
      MaxProcessingTime = TimeSpan.FromMinutes(60.0)
      MinCapacityNeeded = 1
      MaxCapacityNeeded = 1
      MinPrice = 1.0
      MaxPrice = 10000.0
      MinUnitsPerYear = 100
      MaxUnitsPerYear = 1000
      ProductCount = 20  // 120
      JobCount = 1000
      SlowMachinesMinSpeedFactor = 0.3
      SlowMachinesMaxSpeedFactor = 0.7 }

//let flexData = generateJobShopData 1 p
let flexData = generateJobShopDataFromRealData 42 p
FlexibleJobShopData.writeDataToFiles OUTPUTDIRECTORY flexData





// ======================================
// Scheduling
// transform FlexibleJobShopData -> JobShopData
// ======================================



type MakeSchedule = FlexibleJobShopData -> JobShopData

//type FlexibleJobShopData = 
//    { Stages : Repository<Stage Id, Stage>
//      Machines : Repository<Machine Id, Machine>
//      Products : Repository<Product Id, Product>
//      Jobs : Repository<Job Id, Job> }

//type JobShopData = 
//    { Machines : Repository<Machine Id, Machine>
//      Products : Repository<Product Id, Product>
//      Jobs : Repository<Job Id, Job> }



let data0 = JobShopData.create()

let (machineMap, data1) =
    flexData
    |> FlexibleJobShopData.getAllMachines
    |> Seq.fold (fun (map, data) flexMachine -> 
        JobShopData.makeMachine (flexMachine.Capacity, flexMachine.InputBufferCapacity) data
        |> fun (machineId, data) -> 
            (Map.add flexMachine.Id machineId map, data)
        ) (Map.empty, data0)

let (productMap, data2) =
    flexData
    |> FlexibleJobShopData.getAllProducts
    |> Seq.fold (fun (map, data) flexProduct -> 
        JobShopData.makeProduct (flexProduct.Price, flexProduct.Cost, flexProduct.UnitsPerYear) data
        |> fun (productId, data) -> 
            (Map.add flexProduct.Id productId map, data)
        ) (Map.empty, data1)



let rnd = Random.makeGenerator 42

let allocateTask rnd flexData flexTask =
    flexData
    |> FlexibleJobShopData.getAllMachines
    |> Seq.filter (fun flexMachine -> flexMachine.StageId = flexTask.StageId)
    |> Seq.toArray
    |> Random.sampleOne rnd
    |> fun flexMachine -> flexMachine.Id





let (jobMap, data) =
    flexData
    |> FlexibleJobShopData.getAllJobs
    |> Seq.fold (fun (map, data) flexJob -> 
        let tasklist = 
        //PLAN: get productId -> get Product -> get tasklist -> allocate tasks -> flexMachineId ....



            flexProduct.FlexibleTasks
//            |> List.map (fun flexTask -> 
//                let machineId = flexTask.StageId
//                let rank = flexTask.Rank
//                let processingTime = flexTask.BaseProcessingTime //TODO multiply be SpeedFactor
//                let capacityNeeded = flexTask.CapacityNeeded
//                JobShopData.makeTask (machineId, rank, processingTime, capacityNeeded) data
//                )
        let productId = Map.find flexJob.ProductId productMap
        JobShopData.makeJob (productId, tasklist, flexJob.ReleaseDate, flexJob.DueDate) data
        |> fun (jobId, data) -> 
            (Map.add flexJob.Id jobId map, data)
        ) (Map.empty, data2)








(******************** THIS IS WORK IN PROGRESS!!!!! **************)

let makeRandomSchedule (flexData : FlexibleJobShopData) =
    let data0 = JobShopData.create()
    // scheduling is allocation and sequencing
    // scheduling jobs means that all jobs must be allocated to specific 
    // machines in a specific sequence




    // make non-flexible machines
    let data1 =
        FlexibleJobShopData.getAllMachines flexData
        |> Seq.filter (fun machine -> machine.SpeedFactor = 1.0)
        |> Seq.fold (fun data machine -> JobShopData.makeMachine (machine.Capacity, machine.InputBufferCapacity) data |> snd) data0
    // make non-flexible tasks

    // make non-flexible products
    let data2 =
        FlexibleJobShopData.getAllProducts 
    


    flexData











// ======================================
// Simulation
// ======================================
#load "Engine.fs"
open SimJobShop.Engine
#load "JobShopModelOutputBuffers.fs"
open SimJobShop.JobShopModelOutputBuffers


let mutable eventsLog = List.empty<Event>
let saveEvent event = eventsLog <- event::eventsLog
let log = ignore // printfn "%s"

let initial = initSimulation data

#time
let final = Simulation.run saveEvent log initial
#time

let r = eventsLog |> Seq.rev |> Event.writeEventsToFile "\t" (OUTPUTDIRECTORY + """\events.txt""")





