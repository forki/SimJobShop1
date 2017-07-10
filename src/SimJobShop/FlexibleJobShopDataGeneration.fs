module SimJobShop.FlexibleJobShopDataGeneration

open System
open Common
open FlexibleJobShopData

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
      JobCount : int
      SlowMachinesMinSpeedFactor : double
      SlowMachinesMaxSpeedFactor : double }

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

let generatePriceAndCost (rnd : Random) p =
    let price = Random.uniform rnd p.MinPrice p.MinPrice
    (price, 0.5 * price)

let generateUnitsPerYear (rnd : Random) p = Random.int rnd p.MinUnitsPerYear p.MaxUnitsPerYear |> uint32

let generateSpeedFactor (rnd : Random) p = Random.uniform rnd p.SlowMachinesMinSpeedFactor p.SlowMachinesMaxSpeedFactor 

let generateStageTaskList (rnd : Random) p count data = 
    let stageIds = 
        data
        |> FlexibleJobShopData.getAllStageIds
        |> Seq.toArray
    if count > Array.length stageIds then 
        failwith "Cannot generate more tasks than there are machines in the job shop."
    stageIds
    |> Random.sampleNoReplace rnd count
    |> Seq.indexed
    |> Seq.map (fun (rank, machineId) -> 
            (machineId, (uint32) rank, generateProcessingTime rnd p, generateCapacityNeeded rnd p)
            |> fun taskData -> FlexibleJobShopData.makeStageTask taskData data)
    |> Seq.toList

let generateStagesAndMachines (rnd : Random) p data = 
    let data' =
        FlexibleJobShopData.addStages p.MachineCount data
        |> FlexibleJobShopData.addDefaultMachines
    FlexibleJobShopData.getAllMachines data'
    |> Seq.map (fun machine ->
        (machine.StageId, machine.Capacity, generateSpeedFactor rnd p, machine.InputBufferCapacity))
    |> Seq.fold (fun fjsData machineData -> FlexibleJobShopData.makeMachine machineData fjsData |> snd) data'


let generateProducts (rnd : Random) p data = 
    let generateProductData _ = 
        let taskCount = generateTaskCount rnd p
        let stageTaskList = generateStageTaskList rnd p taskCount data
        let price, cost = generatePriceAndCost rnd p
        let units = generateUnitsPerYear rnd p
        (stageTaskList, price, cost, units)
    let folder jsData productData = FlexibleJobShopData.makeProduct productData jsData |> snd
    Seq.init p.ProductCount generateProductData |> Seq.fold folder data

let generateJobs (rnd : Random) p data = 
    let releaseDate = DateTime.Today
    let dueDate = releaseDate.AddDays(2.0)
    let folder jsData jobData = FlexibleJobShopData.makeJob jobData jsData |> snd
    FlexibleJobShopData.getAllProductIds data
    |> Seq.toArray
    |> Random.sampleReplace rnd p.JobCount
    |> Array.toSeq
    |> Seq.map (fun productId -> (productId, releaseDate, dueDate))
    |> Seq.fold folder data

let generateJobShopData seed p = 
    let rnd = Random.makeGenerator seed
    FlexibleJobShopData.create()
    |> generateStagesAndMachines rnd p
    |> generateProducts rnd p
    |> generateJobs rnd p
