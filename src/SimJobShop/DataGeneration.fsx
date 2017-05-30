open System
#load "Common.fs"
open SimJobShop.Common
#load "JobShopData.fs"
open SimJobShop.JobShopData


// ======================================
// Simple example
// ======================================
//let d = 
//    JobShopData.create ()
//    |> JobShopData.addDefaultMachines 5
//let r = JobShopData.writeMachinesToFile sep """c:\temp\machines.txt""" d
//
//let m1 = JobShopData.getAllMachineIds d |> Seq.head
//let m2 = JobShopData.getAllMachineIds d |> Seq.take 2  |> Seq.last
//let t1 = JobShopData.makeTask (m1, 0u, 10.0<second>, FiniteCapacity 1u) d
//let t2 = JobShopData.makeTask (m2, 0u, 5.0<second>, FiniteCapacity 1u) d
//let (p1, d2) = JobShopData.makeProduct ([t1;t2], 2.5m, 250u) d
//let (p2, d3) = JobShopData.makeProduct ([t2;t1], 3.0m, 120u) d2




// ======================================
// Parameters
// ======================================
let machineCount = 10  //20
let minTaskCount = 5
let maxTaskCount = 8 //15
let minProcessingTime = 20.0  // minutes
let maxProcessingTime = 30.0  // minutes
let minCapacityNeeded = 1
let maxCapacityNeeded = 1
let minPrice = 1.0
let maxPrice = 10.0
let minUnitsPerYear = 100
let maxUnitsPerYear = 1000
let productCount = 20  //120
let jobCount = 1000



// ======================================
// Functions to generate the data
// ======================================
let generateProcessingTime (rnd:Random) =
    Random.uniform rnd minProcessingTime maxProcessingTime
    |> TimeSpan.FromMinutes
    
let generateCapacityNeeded (rnd:Random) =
    Random.int rnd minCapacityNeeded maxCapacityNeeded |> uint32 |> FiniteCapacity

let generateTaskCount (rnd:Random) =
    if minTaskCount < 1 then failwith "You need to generate at least one task."
    if minTaskCount > maxTaskCount then failwith "minTaskCount cannot be larger than maxTaskCount."
    Random.int rnd minTaskCount maxTaskCount

let generatePrice (rnd:Random) =
    Random.uniform rnd minProcessingTime maxProcessingTime |> decimal

let generateUnitsPerYear (rnd:Random) =
    Random.int rnd minUnitsPerYear maxUnitsPerYear |> uint32

let generateTaskList (rnd:Random) count jobShopData =
    let machineIds = jobShopData |> JobShopData.getAllMachineIds |> Seq.toArray
    if count > Array.length machineIds then failwith "Cannot generate more tasks than there are machines in the job shop."
    machineIds
    |> Random.sampleNoReplace rnd count
    |> Seq.indexed
    |> Seq.map (fun (rank,machineId) -> (machineId, (uint32)rank, generateProcessingTime rnd, generateCapacityNeeded rnd))
    |> Seq.map (fun data -> JobShopData.makeTask data jobShopData)
    |> Seq.toList

let generateProducts (rnd:Random) count jobShopData =
    let generateProductData _ =
        let taskCount = generateTaskCount rnd
        let taskList = generateTaskList rnd taskCount jobShopData
        let price = generatePrice rnd
        let units = generateUnitsPerYear rnd
        (taskList, price, units)
    let folder jsData productData = JobShopData.makeProduct productData jsData |> snd
    Seq.init count generateProductData
    |> Seq.fold folder jobShopData

let generateJobs (rnd:Random) count jobShopData =
    let releaseDate = DateTime.Today
    let dueDate = releaseDate.AddDays(2.0)
    let folder jsData jobData = JobShopData.makeJob jobData jsData |> snd
    JobShopData.getAllProductIds jobShopData
    |> Seq.toArray
    |> Random.sampleReplace rnd count
    |> Array.toSeq
    |> Seq.map (fun productId -> (productId, releaseDate, dueDate))
    |> Seq.fold folder jobShopData





// ======================================
// Generate data
// ======================================
let rnd = Random.makeGenerator 1
let data =
    JobShopData.create ()
    |> JobShopData.addDefaultMachines machineCount
    |> generateProducts rnd productCount
    |> generateJobs rnd jobCount


JobShopData.writeDataToFiles """C:\Temp\test""" data


