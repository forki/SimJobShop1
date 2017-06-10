module SimJobShop.JobShopDataGeneration

open System
open Common
open JobShopData

type Parameters = 
    { MachineCount : int
      MinTaskCount : int
      MaxTaskCount : int
      MinProcessingTime : TimeSpan
      MaxProcessingTime : TimeSpan
      MinCapacityNeeded : int
      MaxCapacityNeeded : int
      MinPrice : float
      MaxPrice : float
      MinUnitsPerYear : int
      MaxUnitsPerYear : int
      ProductCount : int
      JobCount : int }

let generateProcessingTime (rnd : Random) p = 
    let min = p.MinProcessingTime.TotalMinutes
    let max = p.MaxProcessingTime.TotalMinutes
    Random.uniform rnd min max |> TimeSpan.FromMinutes

let generateCapacityNeeded (rnd : Random) p = 
    Random.int rnd p.MinCapacityNeeded p.MaxCapacityNeeded
    |> uint32
    |> FiniteCapacity

let generateTaskCount (rnd : Random) p = 
    if p.MinTaskCount < 1 then failwith "You need to generate at least one task."
    if p.MinTaskCount > p.MaxTaskCount then failwith "minTaskCount cannot be larger than maxTaskCount."
    Random.int rnd p.MinTaskCount p.MaxTaskCount

let generatePrice (rnd : Random) p = Random.uniform rnd p.MinPrice p.MinPrice |> decimal
let generateUnitsPerYear (rnd : Random) p = Random.int rnd p.MinUnitsPerYear p.MaxUnitsPerYear |> uint32

let generateTaskList (rnd : Random) p count jobShopData = 
    let machineIds = 
        jobShopData
        |> JobShopData.getAllMachineIds
        |> Seq.toArray
    if count > Array.length machineIds then 
        failwith "Cannot generate more tasks than there are machines in the job shop."
    machineIds
    |> Random.sampleNoReplace rnd count
    |> Seq.indexed
    |> Seq.map 
           (fun (rank, machineId) -> 
           (machineId, (uint32) rank, generateProcessingTime rnd p, generateCapacityNeeded rnd p))
    |> Seq.map (fun data -> JobShopData.makeTask data jobShopData)
    |> Seq.toList

let generateMachines (rnd : Random) p jobShopData = 
    JobShopData.addDefaultMachines p.MachineCount jobShopData

let generateProducts (rnd : Random) p jobShopData = 
    let generateProductData _ = 
        let taskCount = generateTaskCount rnd p
        let taskList = generateTaskList rnd p taskCount jobShopData
        let price = generatePrice rnd p
        let units = generateUnitsPerYear rnd p
        (taskList, price, units)
    
    let folder jsData productData = JobShopData.makeProduct productData jsData |> snd
    Seq.init p.ProductCount generateProductData |> Seq.fold folder jobShopData

let generateJobs (rnd : Random) p jobShopData = 
    let releaseDate = DateTime.Today
    let dueDate = releaseDate.AddDays(2.0)
    let folder jsData jobData = JobShopData.makeJob jobData jsData |> snd
    JobShopData.getAllProductIds jobShopData
    |> Seq.toArray
    |> Random.sampleReplace rnd p.JobCount
    |> Array.toSeq
    |> Seq.map (fun productId -> (productId, releaseDate, dueDate))
    |> Seq.fold folder jobShopData

let generateJobShopData seed p = 
    let rnd = Random.makeGenerator seed
    JobShopData.create()
    |> generateMachines rnd p
    |> generateProducts rnd p
    |> generateJobs rnd p
