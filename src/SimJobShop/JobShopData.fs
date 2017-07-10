module SimJobShop.JobShopData

open System
open System.IO
open Common

(*
Invariants in the JobShopData Aggregate
- Machines: MachineIds need to be unique
- Tasks: MachineId must exist in Shop; CapacityNeeded <= Machine.Capacity
- Products: ProductIds must be unique
- Jobs: JobIds must be unique; Jobs are processed in order of the job list.
*)


type Machine = 
    { Id : Machine Id
      Capacity : Capacity
      InputBufferCapacity : Capacity
      }

type Task = 
    { MachineId : Machine Id
      Rank : uint32
      ProcessingTime : TimeSpan
      CapacityNeeded : Capacity }

type Product = 
    { Id : Product Id
      Tasks : Task list
      Price : float
      UnitsPerYear : uint32 }

type Job = 
    { Id : Job Id
      ProductId : Product Id
      ReleaseDate : DateTime
      DueDate : DateTime }

type JobShopData = 
    { Machines : Repository<Machine Id, Machine>
      Products : Repository<Product Id, Product>
      Jobs : Repository<Job Id, Job> }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Machine = 
    let create id capacity inputBufferSize = 
        if Capacity.isZero capacity then failwith "The capacity of a machine cannot be zero."
        { Id = id
          Capacity = capacity
          InputBufferCapacity = inputBufferSize }
    
    let csvHeader separator = [ "Id"; "Capacity"; "InputBufferCapacity" ] |> String.concat separator
    
    let csvRecord separator (machine : Machine) = 
        [ Id.print machine.Id
          Capacity.print machine.Capacity
          Capacity.print machine.InputBufferCapacity ]
        |> String.concat separator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Task = 
    let create machineId rank processingTime capacityNeeded = 
        if processingTime < TimeSpan.Zero then failwith "A task cannot have negative processing time."
        { MachineId = machineId
          Rank = rank
          ProcessingTime = processingTime
          CapacityNeeded = capacityNeeded }
    
    let csvHeader separator = 
        [ "ProductId"; "Rank"; "MachineId"; "ProcessingTime"; "CapacityNeeded" ] |> String.concat separator
    
    let csvRecord separator productId (task : Task) = 
        [ Id.print productId
          sprintf "%i" task.Rank
          Id.print task.MachineId
          sprintf "%f" task.ProcessingTime.TotalMinutes
          Capacity.print task.CapacityNeeded ]
        |> String.concat separator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Product = 
    let create id taskList price unitsPerYear = 
        if List.length taskList < 1 then failwith "A product needs at least one task."
        if price < 0.0 then failwith "The price of a product cannot be negative."
        if unitsPerYear < 1u then failwith "A product must be sold at least once a year."
        { Id = id
          Tasks = taskList
          Price = price
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
module JobShopData = 
    let getAllMachines jobShopData = Repository.getAllItems jobShopData.Machines
    let getAllMachineIds jobShopData = Repository.getAllIds jobShopData.Machines
    let getAllProducts jobShopData = Repository.getAllItems jobShopData.Products
    let getAllProductIds jobShopData = Repository.getAllIds jobShopData.Products
    let getAllJobs jobShopData = Repository.getAllItems jobShopData.Jobs
    let getAllJobIds jobShopData = Repository.getAllIds jobShopData.Jobs
    let getMachine id jobShopData = Repository.get id jobShopData.Machines
    let getProduct id jobShopData = Repository.get id jobShopData.Products
    let getJob id jobShopData = Repository.get id jobShopData.Jobs
    let private containsMachineId id jobShopData = Repository.containsId id jobShopData.Machines
    let private containsProductId id jobShopData = Repository.containsId id jobShopData.Products
    let private containsJobId id jobShopData = Repository.containsId id jobShopData.Jobs
    
    let create() = 
        { Machines = Repository.createDefault<Machine>()
          Products = Repository.createDefault<Product>()
          Jobs = Repository.createDefault<Job>() }
    
    let writeMachinesToFile separator (path : string) jobShopData = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Machine.csvHeader separator)
            getAllMachines jobShopData
            |> Seq.map (Machine.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeProductsToFile separator (path : string) jobShopData = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Product.csvHeader separator)
            getAllProducts jobShopData
            |> Seq.map (Product.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeTasksToFile separator (path : string) jobShopData = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Task.csvHeader separator)
            getAllProducts jobShopData
            |> Seq.collect (Product.csvRecordsOfTasks separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message
    
    let writeJobsToFile separator (path : string) jobShopData = 
        try 
            use sw = new StreamWriter(path)
            sw.WriteLine(Job.csvHeader separator)
            getAllJobs jobShopData
            |> Seq.map (Job.csvRecord separator)
            |> Seq.iter sw.WriteLine
            |> Success
        with ex -> Failure ex.Message

    let writeDataToFiles outFolder jobShopData =
        Directory.CreateDirectory(outFolder) |> ignore
        let machinesFilename = Path.Combine(outFolder, "machines.txt")
        let productsFilename = Path.Combine(outFolder, "products.txt")
        let tasksFilename = Path.Combine(outFolder, "tasks.txt")
        let jobsFilename = Path.Combine(outFolder, "jobs.txt")
        let separator = "\t"
        [ writeMachinesToFile separator machinesFilename jobShopData
          writeProductsToFile separator productsFilename jobShopData
          writeTasksToFile separator tasksFilename jobShopData
          writeJobsToFile separator jobsFilename jobShopData ]
        |> List.filter (function | Success _ -> false | Failure _ -> true)
        |> List.map (function | Success _ -> "" | Failure e -> e)
        |> String.concat Environment.NewLine
        |> function
            | "" -> "Data written successfully to: " + outFolder
            | errors -> "Errors occured while writing to: " + outFolder + Environment.NewLine + errors
        |> printfn "%s"
    
    let makeMachine (capacity, inputBufferSize) jobShopData = 
        let factory id = Machine.create id capacity inputBufferSize
        let (newId, newRepo) = Repository.insert factory jobShopData.Machines
        (newId, { jobShopData with Machines = newRepo })
    
    let makeTask (machineId, rank, processingTime, capacityNeeded) jobShopData = 
        if not (containsMachineId machineId jobShopData) then failwith "The machineId for this task does not exist."
        Task.create machineId rank processingTime capacityNeeded
    
    let makeProduct (taskList, price, unitsPerYear) jobShopData = 
        let factory id = Product.create id taskList price unitsPerYear
        let (newId, newRepo) = Repository.insert factory jobShopData.Products
        (newId, { jobShopData with Products = newRepo })
    
    let makeJob (productId, releaseDate, dueDate) jobShopData = 
        if not (containsProductId productId jobShopData) then failwith "The productId for this job does not exist."
        let factory id = Job.create id productId releaseDate dueDate
        let (newId, newRepo) = Repository.insert factory jobShopData.Jobs
        (newId, { jobShopData with Jobs = newRepo })
    
    let addDefaultMachines count jobShopData = 
        let defaultMachineData = (FiniteCapacity 1u, FiniteCapacity 0u)
        let folder jobShopData machineData = makeMachine machineData jobShopData |> snd
        Seq.init count (fun _ -> defaultMachineData) |> Seq.fold folder jobShopData

    let getEarliestReleaseDate jobShopData =
        getAllJobs jobShopData
        |> Seq.minBy (fun job -> job.ReleaseDate)
        |> fun job -> job.ReleaseDate
