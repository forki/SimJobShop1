module SimJobShop.FlexibleJobShopDataGeneration

open System
open Common
open FlexibleJobShopData
open FSharp.ExcelProvider

type RealDataProvider = ExcelFile< """RealData.xlsx""" >

type Parameters = 
    { Phases : int list
      MachinesPerStage : int
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
      ThreasholdUnitsPerYear : int
      SlowMachinesMinSpeedFactor : double
      SlowMachinesMaxSpeedFactor : double }

let generateProcessingTime (rnd : Random) p scale = 
    let min = p.MinProcessingTime.TotalMinutes
    let max = p.MaxProcessingTime.TotalMinutes
    Random.uniform rnd min max
    |> (*) scale
    |> TimeSpan.FromMinutes

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
    if count > Array.length stageIds then failwith "Cannot generate more tasks than there are machines in the job shop."
    stageIds
    |> Random.sampleNoReplace rnd count
    |> Seq.indexed
    |> Seq.map 
           (fun (rank, stageId) -> 
           (stageId, (uint32) rank, generateProcessingTime rnd p 1.0, generateCapacityNeeded rnd p) 
           |> fun taskData -> FlexibleJobShopData.makeTask taskData data)
    |> Seq.toList

let generateStagesAndMachines (rnd : Random) p data = 
    let data' = FlexibleJobShopData.addStages p.Phases data
    let capacity = FiniteCapacity 1u
    let inputBufferSize = FiniteCapacity 1u
    FlexibleJobShopData.getAllStageIds data'
    |> Seq.collect (fun stageId -> Seq.init p.MachinesPerStage (fun _ -> stageId))
    |> Seq.mapi (fun i stageId -> 
           let speedFactor = 
               if (i % p.MachinesPerStage = 0) then 1.0
               else generateSpeedFactor rnd p
           (stageId, capacity, speedFactor, inputBufferSize))
    |> Seq.fold (fun data machineData -> 
           data
           |> FlexibleJobShopData.makeMachine machineData
           |> snd) data'

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

let generateProductsFromRealData (rnd : Random) p data = 
    let file = new RealDataProvider()
    let nBins = 4
    
    let (getIndexV, getIndexM, getIndexH) = 
        file.Data
        |> Seq.map (fun row -> row.Geschwindigkeit, row.Gewicht, row.Höhe)
        |> Seq.filter (fun (v, m, h) -> v > 0.0 || m > 0.0 || h > 0.0)
        |> Seq.toArray
        |> Array.unzip3
        |> fun (vs, ms, hs) -> 
            (MoreMath.makeBinning nBins vs, MoreMath.makeBinning nBins ms, MoreMath.makeBinning nBins hs)
    
    let meanCost = file.Data |> Seq.averageBy (fun row -> row.``Cost per unit``)
    let stdCost =
        file.Data 
        |> Seq.sumBy (fun row -> (row.``Cost per unit`` - meanCost)**2.0)
        |> fun x -> x / (float)(Seq.length file.Data) |> sqrt
    let getProcessingTime (rnd : Random) p c =
        let x =
            if c < meanCost - 0.5 * stdCost then Random.uniform rnd 0.0 1.0
            else if c > meanCost + 0.5 * stdCost then Random.uniform rnd 1.0 2.0
            else Random.uniform rnd 0.5 1.5
            |> round |> (*) 0.5
        p.MinProcessingTime.TotalMinutes + x * (p.MaxProcessingTime.TotalMinutes - p.MinProcessingTime.TotalMinutes)
        |> TimeSpan.FromMinutes
    
    // helpers
    let arrayIndexFilter filter = 
        Array.indexed
        >> Array.filter (fst >> filter)
        >> Array.map snd
    
    let limit lower upper = max lower >> min upper
    
    let generateFlexibleTaskList (row : RealDataProvider.Row) jsData = 
        jsData
        |> FlexibleJobShopData.getAllStages
        |> Seq.toArray
        |> Array.groupBy (fun stage -> stage.Phase)
        |> Array.map (fun (phase, stages) -> 
               match phase with
               | 1 | 5 -> 
                   stages
                   |> Random.sampleNoReplace rnd 2
                   |> Array.sortBy (fun stage -> stage.Id |> Id.value)
               | 2 -> 
                   let bin = row.Geschwindigkeit |> getIndexV
                   stages |> arrayIndexFilter (fun i -> i <> bin)
               | 3 -> 
                   let bin = row.Gewicht |> getIndexM
                   stages |> arrayIndexFilter (fun i -> i <> bin)
               | 4 -> 
                   let bin = row.Höhe |> getIndexH
                   stages |> arrayIndexFilter (fun i -> i <> bin)
               | _ -> failwith "Only 5 stages are allowed!")
        |> Array.collect id
        |> Array.mapi (fun i stage -> 
            let id = i |> uint32
            let processingTime = getProcessingTime rnd p row.``Cost per unit``
            let capNeeded = generateCapacityNeeded rnd p
            (stage.Id, id, processingTime, capNeeded)
            )
        |> Array.map (fun taskData -> FlexibleJobShopData.makeTask taskData jsData)
        |> Array.toList
    
    let productData = 
        file.Data
        |> Seq.filter 
               (fun row -> row.``Price per unit`` > 0.0 || row.``Cost per unit`` > 0.0 || row.``Units per year`` > 0.0)
        |> Seq.map 
               (fun row -> 
               generateFlexibleTaskList row data, row.``Price per unit``, row.``Cost per unit``, 
               row.``Units per year`` |> uint32)
    
    let makeProductFolder jsData productData = FlexibleJobShopData.makeProduct productData jsData |> snd
    productData |> Seq.fold makeProductFolder data

let generateJobsFromRealData (rnd : Random) p data = 
    let releaseDate = DateTime.Today
    let dueDate = releaseDate.AddDays(5.0)
    let makeJobFolder jsData jobData = FlexibleJobShopData.makeJob jobData jsData |> snd
    data
    |> FlexibleJobShopData.getAllProducts
    |> Seq.map (fun product -> product, (product.UnitsPerYear |> float) / 12.0)
//    |> Seq.filter (fun (_, freq) -> rnd.NextDouble() < freq)
    |> Seq.filter (fun (product, _) -> (int)product.UnitsPerYear >= p.ThreasholdUnitsPerYear)
    |> Seq.map (fun (product, freq) -> product, freq |> max 1.0 |> round |> int)
    |> Seq.collect (fun (product, count) -> Seq.init count (fun _ -> product))
    |> Seq.map (fun product -> (product.Id, releaseDate, dueDate))
    |> Seq.fold makeJobFolder data

let generateJobShopDataFromRealData seed p = 
    let rnd = Random.makeGenerator seed
    FlexibleJobShopData.create()
    |> generateStagesAndMachines rnd p
    |> generateProductsFromRealData rnd p
    |> generateJobsFromRealData rnd p
