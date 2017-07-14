open System
#load "Common.fs"
open SimJobShop.Common
#load "FlexibleJobShopData.fs"
open SimJobShop.FlexibleJobShopData


// ======================================
// Data Generation
// ======================================
#load "FlexibleJobShopDataGeneration.fs"
open SimJobShop.FlexibleJobShopDataGeneration

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
      JobCount = 1000
      SlowMachinesMinSpeedFactor = 0.3
      SlowMachinesMaxSpeedFactor = 0.7 }

let data = generateJobShopData 1 p
FlexibleJobShopData.writeDataToFiles """C:\Users\hols\Projekte\KTI_Complexity-4.0\Test\GeneratedFlexible""" data



// ======================================
// Data from Excel
// ======================================

#r @"..\..\packages\ExcelProvider\lib\ExcelProvider.dll"
open FSharp.ExcelProvider

type SchindlerDataProvider = ExcelFile<"""C:\Users\hols\Projekte\KTI_Complexity-4.0\Daten\Schindler\data.xlsx""">
let file = new SchindlerDataProvider()

let products =
    file.Data
    |> Seq.mapi (fun i row -> 
        i+1 |> uint64 |> Id, row.``Price per unit``, row.``Cost per unit``, row.``Units per year``)
    // |> Seq.map (fun data -> Product.create )
    // |> Seq.take 5 |> Seq.iter (printfn "%A")

let products =
    file.Data
    |> Seq.mapi (fun i row ->
        row.``Price per unit``, row.``Cost per unit``, row.Geschwindigkeit, row.Höhe, row.Gewicht)

let n = Seq.length products
let m = Seq.distinct products |> Seq.length






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
    let data1 =
        FlexibleJobShopData.getAllMachines flexData
        |> Seq.filter (fun machine -> machine.SpeedFactor = 1.0)
        |> Seq.fold (fun machine data -> JobShopData.makeMachine (machine.Capacity, machine.InputBufferSize) data |> snd) data0


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


let r = eventsLog |> Seq.rev |> Event.writeEventsToFile "\t" """C:\Users\hols\Projekte\KTI_Complexity-4.0\Test\Generated\events.txt""" 
