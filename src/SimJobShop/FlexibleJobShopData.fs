module SimJobShop.FlexibleJobShopData

open System
open System.IO
open Common

(*
Invariants in the FlexibleJobShopData Aggregate
- MachineTypes: MachineTypes must be unique
- Machines: MachineIds need to be unique; MachineTypes must exist
- Tasks: MachineId must exist in Shop; CapacityNeeded <= Machine.Capacity
- Products: ProductIds must be unique
- Jobs: JobIds must be unique; Jobs are processed in order of the job list.
*)

type Stage = { Id : Stage Id }

type Machine = 
    { Id : Machine Id
      StageId : Stage Id
      Capacity : Capacity
      SpeedFactor : float  // -> 1, [0.5 1]
      InputBufferCapacity : Capacity }

type Task = 
    { StageId : Stage Id
      Rank : uint32
      BaseProcessingTime : TimeSpan
      CapacityNeeded : Capacity }

type Product = 
    { Id : Product Id
      Tasks : Task list
      Price : float
      Cost : float
      UnitsPerYear : uint32 }

type Job = 
    { Id : Job Id
      ProductId : Product Id
      ReleaseDate : DateTime
      DueDate : DateTime }

type FlexibleJobShopData = 
    { Stages : Repository<Stage Id, Stage>
      Machines : Repository<Machine Id, Machine>
      Products : Repository<Product Id, Product>
      Jobs : Repository<Job Id, Job> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Stage = 
    let create id = 
        { Stage.Id = id }
    
    let csvHeader separator = [ "Id" ] |> String.concat separator
    
    let csvRecord separator (stage : Stage) = 
        [ Id.print stage.Id ]
        |> String.concat separator


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Machine = 
    let create id stageId capacity speedFactor inputBufferSize = 
        if Capacity.isZero capacity then failwith "The capacity of a machine cannot be zero."
        { Id = id
          StageId = stageId
          Capacity = capacity
          SpeedFactor = speedFactor
          InputBufferCapacity = inputBufferSize }
    
    let csvHeader separator = [ "Id"; "Capacity"; "InputBufferCapacity" ] |> String.concat separator
    
    let csvRecord separator (machine : Machine) = 
        [ Id.print machine.Id
          Capacity.print machine.Capacity
          Capacity.print machine.InputBufferCapacity ]
        |> String.concat separator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Task = 
    let create stageId rank baseProcessingTime capacityNeeded = 
        if baseProcessingTime < TimeSpan.Zero then failwith "A task cannot have negative processing time."
        { StageId = stageId
          Rank = rank
          BaseProcessingTime = baseProcessingTime
          CapacityNeeded = capacityNeeded }
    
    let csvHeader separator = 
        [ "ProductId"; "Rank"; "StageId"; "BaseProcessingTime"; "CapacityNeeded" ] |> String.concat separator
    
    let csvRecord separator productId (task : Task) = 
        [ Id.print productId
          sprintf "%i" task.Rank
          Id.print task.StageId
          sprintf "%f" task.BaseProcessingTime.TotalMinutes
          Capacity.print task.CapacityNeeded ]
        |> String.concat separator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Product = 
    let create id taskList price cost unitsPerYear = 
        if List.length taskList < 1 then failwith "A product needs at least one task."
        if price < 0.0 then failwith "The price of a product cannot be negative."
        if cost < 0.0 then failwith "The cost of a product cannot be negative."
        if unitsPerYear < 1u then failwith "A product must be sold at least once a year."
        { Id = id
          Tasks = taskList
          Price = price
          Cost = cost
          UnitsPerYear = unitsPerYear }
    
    let getFactory taskList price unitsPerYear = fun id -> create id taskList price unitsPerYear
    let csvHeader separator = [ "Id"; "Price"; "UnitsPerYear" ] |> String.concat separator
    
    let csvRecord separator (product : Product) = 
        [ Id.print product.Id
          sprintf "%.2f" product.Price
          sprintf "%i" product.UnitsPerYear ]
        |> String.concat separator
    
    let csvRecordsOfTasks separator (product : Product) = 
        let taskToRecord = Task.csvRecord separator product.Id
        product.Tasks |> Seq.map taskToRecord

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Job = 
    let create id productId releaseDate dueDate = 
        { Id = id
          ProductId = productId
          ReleaseDate = releaseDate
          DueDate = dueDate }
    
    let getFactory productId releaseDate dueDate = fun id -> create id productId releaseDate dueDate
    let csvHeader separator = [ "Id"; "ProductId"; "ReleaseDate"; "DueDate" ] |> String.concat separator
    
    let csvRecord separator (job : Job) = 
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
          Machines = Repository.createDefault<Machine>()
          Products = Repository.createDefault<Product>()
          Jobs = Repository.createDefault<Job>() }
    
    let writeStagesToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Stage.csvHeader separator)
            getAllMachines data
            |> Seq.map (Machine.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeMachinesToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Machine.csvHeader separator)
            getAllMachines data
            |> Seq.map (Machine.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeProductsToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Product.csvHeader separator)
            getAllProducts data
            |> Seq.map (Product.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeTasksToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Task.csvHeader separator)
            getAllProducts data
            |> Seq.collect (Product.csvRecordsOfTasks separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeJobsToFile separator (path : string) data = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Job.csvHeader separator)
            getAllJobs data
            |> Seq.map (Job.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message

    let writeDataToFiles outFolder data =
        Directory.CreateDirectory(outFolder) |> ignore
        let stagesFilename = Path.Combine(outFolder, "stages.txt")
        let machinesFilename = Path.Combine(outFolder, "machines.txt")
        let productsFilename = Path.Combine(outFolder, "products.txt")
        let tasksFilename = Path.Combine(outFolder, "tasks.txt")
        let jobsFilename = Path.Combine(outFolder, "jobs.txt")
        let separator = "\t"
        [ writeStagesToFile separator stagesFilename data
          writeMachinesToFile separator machinesFilename data
          writeProductsToFile separator productsFilename data
          writeProductsToFile separator productsFilename data
          writeTasksToFile separator tasksFilename data
          writeJobsToFile separator jobsFilename data ]
        |> List.filter (function | Success _ -> false | Failure _ -> true)
        |> List.map (function | Success _ -> "" | Failure e -> e)
        |> String.concat Environment.NewLine
        |> function
            | "" -> "Data written successfully to: " + outFolder
            | errors -> "Errors occured while writing to: " + outFolder + Environment.NewLine + errors
        |> printfn "%s"

    let makeStage data =
        let factory id = Stage.create id
        let (newId, newRepo) = Repository.insert factory data.Stages
        (newId, { data with Stages = newRepo })

    
    let makeMachine (stageId, capacity, speedFactor, inputBufferSize) data = 
        let factory id = Machine.create id stageId capacity speedFactor inputBufferSize
        let (newId, newRepo) = Repository.insert factory data.Machines
        (newId, { data with Machines = newRepo })
    
    let makeTask (stageId, rank, baseProcessingTime, capacityNeeded) data = 
        if not (containsStageId stageId data) then failwith "The machineId for this task does not exist."
        Task.create stageId rank baseProcessingTime capacityNeeded
    
    let makeProduct (taskList, price, cost, unitsPerYear) data = 
        let factory id = Product.create id taskList price cost unitsPerYear
        let (newId, newRepo) = Repository.insert factory data.Products
        (newId, { data with Products = newRepo })
    
    let makeJob (productId, releaseDate, dueDate) data = 
        if not (containsProductId productId data) then failwith "The productId for this job does not exist."
        let factory id = Job.create id productId releaseDate dueDate
        let (newId, newRepo) = Repository.insert factory data.Jobs
        (newId, { data with Jobs = newRepo })

    let addDefaultStages count data =
        let folder data _ = makeStage data |> snd
        Seq.init count (fun _ -> ()) |> Seq.fold folder data
    
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
