module SimJobShop.FlexibleJobShopData

open System
open System.IO
open Common

(*
Invariants in the FlexibleJobShopData Aggregate
- Machines: MachineIds need to be unique;
- StageTasks: MachineId must exist in Shop; CapacityNeeded <= Machine.Capacity
- Products: ProductIds must be unique
- Jobs: JobIds must be unique; Jobs are processed in order of the job list.
*)

type Stage = 
    { Id : Stage Id
      Phase : int }

type FlexibleMachine = 
    { Id : FlexibleMachine Id
      StageId : Stage Id
      SpeedFactor : float // -> 1, [0.5 1]
      Capacity : Capacity
      InputBufferCapacity : Capacity }

type FlexibleTask = 
    { StageId : Stage Id
      Rank : uint32
      BaseProcessingTime : TimeSpan
      CapacityNeeded : Capacity }

type FlexibleProduct = 
    { Id : FlexibleProduct Id
      FlexibleTasks : FlexibleTask list
      Price : float
      Cost : float
      UnitsPerYear : uint32 }

type FlexibleJob = 
    { Id : FlexibleJob Id
      ProductId : FlexibleProduct Id
      ReleaseDate : DateTime
      DueDate : DateTime }

type FlexibleJobShopData = 
    { Stages : Repository<Stage Id, Stage>
      Machines : Repository<FlexibleMachine Id, FlexibleMachine>
      Products : Repository<FlexibleProduct Id, FlexibleProduct>
      Jobs : Repository<FlexibleJob Id, FlexibleJob> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Stage = 
    let create id phase = 
        { Stage.Id = id
          Phase = phase }
    
    let csvHeader separator = [ "StageId"; "Phase" ] |> String.concat separator
    
    let csvRecord separator (stage : Stage) = 
        [ Id.print stage.Id
          sprintf "%i" stage.Phase ]
        |> String.concat separator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FlexibleMachine = 
    let create id stageId capacity speedFactor inputBufferSize = 
        if Capacity.isZero capacity then failwith "The capacity of a machine cannot be zero."
        { Id = id
          StageId = stageId
          SpeedFactor = speedFactor
          Capacity = capacity
          InputBufferCapacity = inputBufferSize }
    
    let csvHeader separator = 
        [ "FlexibleMachineId"; "StageId"; "SpeedFactor"; "Capacity"; "InputBufferCapacity" ] |> String.concat separator
    
    let csvRecord separator (machine : FlexibleMachine) = 
        [ Id.print machine.Id
          Id.print machine.StageId
          sprintf "%.2f" machine.SpeedFactor
          Capacity.print machine.Capacity
          Capacity.print machine.InputBufferCapacity ]
        |> String.concat separator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FlexibleTask = 
    let create stageId rank baseProcessingTime capacityNeeded = 
        if baseProcessingTime < TimeSpan.Zero then failwith "A stage-task cannot have negative processing time."
        { StageId = stageId
          Rank = rank
          BaseProcessingTime = baseProcessingTime
          CapacityNeeded = capacityNeeded }
    
    let csvHeader separator = 
        [ "FlexibleProductId"; "Rank"; "StageId"; "BaseProcessingTime_Minutes"; "CapacityNeeded" ] 
        |> String.concat separator
    
    let csvRecord separator productId stageTask = 
        [ Id.print productId
          sprintf "%i" stageTask.Rank
          Id.print stageTask.StageId
          sprintf "%.2f" stageTask.BaseProcessingTime.TotalMinutes
          Capacity.print stageTask.CapacityNeeded ]
        |> String.concat separator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FlexibleProduct = 
    let create id stageTaskList price cost unitsPerYear = 
        if List.length stageTaskList < 1 then failwith "A product needs at least one stage-task."
        if price < 0.0 then failwith "The price of a product cannot be negative."
        if cost < 0.0 then failwith "The cost of a product cannot be negative."
        if unitsPerYear < 1u then failwith "A product must be sold at least once a year."
        { Id = id
          FlexibleTasks = stageTaskList
          Price = price
          Cost = cost
          UnitsPerYear = unitsPerYear }
    
    let getFactory stageTaskList price unitsPerYear = fun id -> create id stageTaskList price unitsPerYear
    let csvHeader separator = [ "FlexibleProductId"; "Price"; "Cost"; "UnitsPerYear" ] |> String.concat separator
    
    let csvRecord separator (product : FlexibleProduct) = 
        [ Id.print product.Id
          sprintf "%.2f" product.Price
          sprintf "%.2f" product.Cost
          sprintf "%i" product.UnitsPerYear ]
        |> String.concat separator
    
    let csvRecordsOfStageTasks separator (product : FlexibleProduct) = 
        let stageTaskToRecord = FlexibleTask.csvRecord separator product.Id
        product.FlexibleTasks |> Seq.map stageTaskToRecord

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FlexibleJob = 
    let create id productId releaseDate dueDate = 
        { Id = id
          ProductId = productId
          ReleaseDate = releaseDate
          DueDate = dueDate }
    
    let getFactory productId releaseDate dueDate = fun id -> create id productId releaseDate dueDate
    let csvHeader separator = 
        [ "FlexibleJobId"; "FlexibleProductId"; "ReleaseDate"; "DueDate" ] |> String.concat separator
    
    let csvRecord separator (job : FlexibleJob) = 
        [ Id.print job.Id
          Id.print job.ProductId
          sprintf "%A" job.ReleaseDate
          sprintf "%A" job.DueDate ]
        |> String.concat separator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FlexibleJobShopData = 
    let getAllStages data = Repository.getAllItems data.Stages
    let getAllStageIds data = Repository.getAllIds data.Stages
    let getAllMachines data = Repository.getAllItems data.Machines
    let getAllMachineIds data = Repository.getAllIds data.Machines
    let getAllProducts data = Repository.getAllItems data.Products
    let getAllProductIds data = Repository.getAllIds data.Products
    let getAllJobs data = Repository.getAllItems data.Jobs
    let getAllJobIds data = Repository.getAllIds data.Jobs
    let getStage id data = Repository.get id data.Stages
    let getMachine id data = Repository.get id data.Machines
    let getProduct id data = Repository.get id data.Products
    let getJob id data = Repository.get id data.Jobs
    let private containsStageId id data = Repository.containsId id data.Stages
    let private containsMachineId id data = Repository.containsId id data.Machines
    let private containsProductId id data = Repository.containsId id data.Products
    let private containsJobId id data = Repository.containsId id data.Jobs
    
    let create() = 
        { Stages = Repository.createDefault<Stage>()
          Machines = Repository.createDefault<FlexibleMachine>()
          Products = Repository.createDefault<FlexibleProduct>()
          Jobs = Repository.createDefault<FlexibleJob>() }
    
    let writeStagesToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Stage.csvHeader separator)
            getAllStages data
            |> Seq.map (Stage.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeMachinesToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(FlexibleMachine.csvHeader separator)
            getAllMachines data
            |> Seq.map (FlexibleMachine.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeProductsToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(FlexibleProduct.csvHeader separator)
            getAllProducts data
            |> Seq.map (FlexibleProduct.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeStageTasksToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(FlexibleTask.csvHeader separator)
            getAllProducts data
            |> Seq.collect (FlexibleProduct.csvRecordsOfStageTasks separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeJobsToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(FlexibleJob.csvHeader separator)
            getAllJobs data
            |> Seq.map (FlexibleJob.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeDataToFiles outFolder data = 
        Directory.CreateDirectory(outFolder) |> ignore
        let stagesFilename = Path.Combine(outFolder, "stages.txt")
        let machinesFilename = Path.Combine(outFolder, "flexiblemachines.txt")
        let productsFilename = Path.Combine(outFolder, "flexibleproducts.txt")
        let stageTasksFilename = Path.Combine(outFolder, "flexibletasks.txt")
        let jobsFilename = Path.Combine(outFolder, "flexiblejobs.txt")
        let separator = "\t"
        [ writeStagesToFile separator stagesFilename data
          writeMachinesToFile separator machinesFilename data
          writeProductsToFile separator productsFilename data
          writeStageTasksToFile separator stageTasksFilename data
          writeJobsToFile separator jobsFilename data ]
        |> List.filter Result.isFailure
        |> List.map (function 
               | Success _ -> ""
               | Failure e -> e)
        |> String.concat Environment.NewLine
        |> function 
        | "" -> "Data written successfully to: " + outFolder
        | errors -> "Errors occured while writing to: " + outFolder + Environment.NewLine + errors
        |> printfn "%s"
    
    let makeStage phase data = 
        let factory id = Stage.create id phase
        let (newId, newRepo) = Repository.insert factory data.Stages
        (newId, { data with Stages = newRepo })
    
    let makeMachine (stageId, capacity, speedFactor, inputBufferSize) data = 
        let factory id = FlexibleMachine.create id stageId capacity speedFactor inputBufferSize
        let (newId, newRepo) = Repository.insert factory data.Machines
        (newId, { data with Machines = newRepo })
    
    let makeTask (stageId, rank, baseProcessingTime, capacityNeeded) data = 
        if not (containsStageId stageId data) then failwith "The stageId for this stage-task does not exist."
        FlexibleTask.create stageId rank baseProcessingTime capacityNeeded
    
    let makeProduct (stageTaskList, price, cost, unitsPerYear) data = 
        let factory id = FlexibleProduct.create id stageTaskList price cost unitsPerYear
        let (newId, newRepo) = Repository.insert factory data.Products
        (newId, { data with Products = newRepo })
    
    let makeJob (productId, releaseDate, dueDate) data = 
        if not (containsProductId productId data) then failwith "The productId for this job does not exist."
        let factory id = FlexibleJob.create id productId releaseDate dueDate
        let (newId, newRepo) = Repository.insert factory data.Jobs
        (newId, { data with Jobs = newRepo })
    
    let addStages phases data = 
        let folder data phase = makeStage phase data |> snd
        phases |> Seq.fold folder data
    
    let addDefaultMachines data = 
        let capacity = FiniteCapacity 1u
        let speedFactor = 1.0
        let inputBufferSize = FiniteCapacity 1u
        let folder data machineData = makeMachine machineData data |> snd
        getAllStageIds data
        |> Seq.map (fun stageId -> (stageId, capacity, speedFactor, inputBufferSize))
        |> Seq.fold folder data
    
    let getEarliestReleaseDate data = 
        getAllJobs data
        |> Seq.minBy (fun job -> job.ReleaseDate)
        |> fun job -> job.ReleaseDate
