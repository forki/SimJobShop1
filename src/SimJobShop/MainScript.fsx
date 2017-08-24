#r @"..\..\packages\ExcelProvider\lib\ExcelProvider.dll"
#load "Common.fs"
#load "FlexibleJobShopData.fs"
#load "FlexibleJobShopDataGeneration.fs"

open System
open SimJobShop.Common
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

//let data = generateJobShopData 1 p
let data = generateJobShopDataFromRealData 42 p
FlexibleJobShopData.writeDataToFiles OUTPUTDIRECTORY data






// ======================================
// Scheduling
// transform FlexibleJobShopData -> JobShopData
// ======================================

#load "JobShopData.fs"
open SimJobShop.JobShopData


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





