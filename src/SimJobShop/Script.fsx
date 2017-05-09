#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"

open FSharp.Data

type JobProvider = CsvProvider< """C:\Users\hols\Projekte\KTI_Complexity-4.0\Code\data_templates\jobs.csv""" >

let jobs = JobProvider.Load("""C:\Users\hols\Projekte\KTI_Complexity-4.0\Code\data_templates\jobs.csv""")

type TaskProvider = CsvProvider< """C:\Users\hols\Projekte\KTI_Complexity-4.0\Code\data_templates\tasks.csv""" >

let tasks = TaskProvider.Load("""C:\Users\hols\Projekte\KTI_Complexity-4.0\Code\data_templates\tasks.csv""")
let printTask (task : TaskProvider.Row) = 
    printfn "=> machine %i, processing time %f, capacity needed %i" task.MachineId task.ProcessingTime 
        task.CapacityNeeded

let printJob (job : JobProvider.Row) = 
    printfn "job %i" job.Id
    for task in tasks.Rows do
        if task.JobId = job.Id then printTask task

tasks.Rows |> Seq.iter printTask
jobs.Rows |> Seq.iter printJob

type SomeTask = 
    { MachineId : int
      CapacityNeeded : int }

let tasklist = 
    [ for task in tasks.Rows -> 
          { MachineId = task.MachineId
            CapacityNeeded = task.CapacityNeeded } ]




[<Measure>]
type min

type 'a Id = Id of uint64

/// A buffer is a FiFo queue with a limited capacity
type 'entity Buffer = 
    { Capacity : int
      Queue : 'entity System.Collections.Generic.Queue }

/// A location is a non-moving place of action with limited capacity, an input buffer and a waitlist of incoming entities.
type 'entity Location = 
    { Id : 'entity Location Id
      Capacity : int
      InputBuffer : 'entity Buffer
      Waitlist : 'entity list }

type 'entity Task = 
    { MachineId : 'entity Location Id
      ProcessingTime : float<min> }

/// A job represents a product to be manufactured in the job shop according to a ordered list of tasks
type Job = 
    { Id : Job Id
      Tasks : Job Task list }

type Machine = Job Location

type Shopfloor = 
    | Shopfloor of Map<Machine Id, Machine>

type JobSequence = 
    | JobSequence of Job list

type Model = 
    { Shopfloor : Shopfloor
      JobSequence : JobSequence }


module Buffer = 
    let Create capacity = 
        { Capacity = capacity
          Queue = new System.Collections.Generic.Queue<'a>() }
    
    let HasCapacity (buffer : 'a Buffer) = buffer.Queue.Count < buffer.Capacity

    let HasEntities (buffer : 'a Buffer) = buffer.Queue.Count > 0

    let IsEmpty (buffer : 'a Buffer) = HasEntities buffer |> not
    
    let Enqueue entity buffer = 
        if HasCapacity buffer then
            let mutable queue = buffer.Queue
            queue.Enqueue entity
            Some { buffer with Queue = queue }
        else None
    
    let Dequeue buffer =
        if HasEntities buffer then
            let mutable queue = buffer.Queue
            let entity = queue.Dequeue()
            Some (entity, { buffer with Queue = queue })
        else None


module Location = 
    let Create id capacity iputBufferCapacity = 
        if capacity < 1 then
            None
        elif iputBufferCapacity < 0 then
            None
        else
            { Id = id
              Capacity = capacity
              InputBuffer = Buffer.Create iputBufferCapacity
              Waitlist = [] }
            |> Some










(** USAGE EXAMPLE **)

// generate data
let products = JobShop.generateProducts
let jobList = JobShop.generateJobs products

// create model
let model = JobShop.createModel products
let schedule = JobShop.jobListToSchedule jobList
  // let JobShop.jobListToSchedule jobList = jobList |> List.map JobShop.jobToCommand |> Schedule


// run the simulation 
open SimJobShop.Engine
let simulation = createInitial model schedule
let results = Simulation.run simulation





