// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load "Library.fs"

open SimJobShop

let num = Library.hello 42

printfn "%i" num

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

type Task = 
    { MachineId : int
      CapacityNeeded : int }

let tasklist = 
    [ for task in tasks.Rows -> 
          { MachineId = task.MachineId
            CapacityNeeded = task.CapacityNeeded } ]
