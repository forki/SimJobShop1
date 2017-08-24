#load "Common.fs"
#load "Engine.fs"
#load "JobShopData.fs"
#load "JobShopModelOutputBuffers.fs"

open System
open SimJobShop.Common
open SimJobShop.Engine
open SimJobShop.JobShopData
open SimJobShop.JobShopModelOutputBuffers


// ======================================
// Test data
// ======================================
let d0 = JobShopData.create () |> JobShopData.addDefaultMachines 2
let machineIds = JobShopData.getAllMachineIds d0
//let r = JobShopData.writeMachinesToFile "\t" """C:\Users\hols\Projekte\KTI_Complexity-4.0\Test""" d
let m1 = machineIds |> Seq.head
let m2 = machineIds |> Seq.skip 1 |> Seq.head
let t1 = JobShopData.makeTask (m1, 0u, TimeSpan.FromHours(1.0), FiniteCapacity 1u) d0
let t2 = JobShopData.makeTask (m2, 1u, TimeSpan.FromHours(2.0), FiniteCapacity 1u) d0
//let p1, d1 = JobShopData.makeProduct ([t1; t2; {t1 with Rank=2u}], 2.5, 250u) d0
let p1, d1 = JobShopData.makeProduct ([t1; t2], 2.5, 250u) d0
let p2, d2 = JobShopData.makeProduct ([{t2 with Rank=0u}; {t1 with Rank=1u}], 3.0, 120u) d1


// some jobs
let data = 
    d2
    |> JobShopData.makeJob (p1, DateTime.Today, DateTime.Today.AddDays(2.0)) |> snd
    |> JobShopData.makeJob (p1, DateTime.Today, DateTime.Today.AddDays(2.0)) |> snd

// write to files
JobShopData.writeDataToFiles """C:\Users\hols\Projekte\KTI_Complexity-4.0\Test""" data


// ======================================
// Simulation
// ======================================
let mutable eventsLog = List.empty<Event>
let saveEvent event = eventsLog <- event::eventsLog
let log = printfn "%s"
let step sim = Simulation.evolveSim saveEvent log sim

let initial = initSimulation data

//let sim1 = step initial
//let sim2 = step sim1
//let sim3 = step sim2
//let sim4 = step sim3
//let sim5 = step sim4
//let sim6 = step sim5
//let sim7 = step sim6
//let sim8 = step sim7
//let sim9 = step sim8
//let sim10 = step sim9

let final = Simulation.run saveEvent log initial

//let r = eventsLog |> Seq.rev |> Event.writeEventsToFile "\t" """C:\Users\hols\Projekte\KTI_Complexity-4.0\Test\Generated\events.txt""" 
let r = eventsLog |> Seq.rev |> Event.writeEventsToFile "\t" """C:\Users\hols\Projekte\KTI_Complexity-4.0\Test\events_now.txt""" 


(*******************

Event '{Time = 12.06.2017 00:00:00;
 Fact = EntityCreated (Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = EntityCreated (Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = EntityEnteredInWaitlist (Id 1UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = EntityEnteredInWaitlist (Id 2UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = EntitySelectedFromWaitlist (Id 1UL,Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = CapacityBlocked (Id 1UL,Id 1UL,FiniteCapacity 1u);}' generated 0 commands
Event '{Time = 12.06.2017 00:00:00;
 Fact = ChangeoverStarted (Id 1UL,Id 1UL,00:30:00);}' generated 1 commands
Event '{Time = 12.06.2017 00:30:00;
 Fact = ChangeoverEnded (Id 1UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 00:30:00;
 Fact = MovedToLocation (Id 1UL,Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 00:30:00;
 Fact = ProcessingStarted (Id 1UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = ProcessingEnded (Id 1UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = EntityEnteredInWaitlist (Id 1UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = EntitySelectedFromWaitlist (Id 1UL,Id 2UL);}' generated 0 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = CapacityBlocked (Id 1UL,Id 2UL,FiniteCapacity 1u);}' generated 0 commands
Event '{Time = 12.06.2017 01:30:00;
 Fact = ChangeoverStarted (Id 1UL,Id 2UL,00:30:00);}' generated 1 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = ChangeoverEnded (Id 1UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = MovedToLocation (Id 1UL,Id 2UL);}' generated 0 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = CapacityReleased (Id 1UL,Id 1UL,FiniteCapacity 1u);}' generated 1 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = ProcessingStarted (Id 1UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = EntitySelectedFromWaitlist (Id 2UL,Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = CapacityBlocked (Id 2UL,Id 1UL,FiniteCapacity 1u);}' generated 0 commands
Event '{Time = 12.06.2017 02:00:00;
 Fact = ChangeoverStarted (Id 2UL,Id 1UL,00:30:00);}' generated 1 commands
Event '{Time = 12.06.2017 02:30:00;
 Fact = ChangeoverEnded (Id 2UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 02:30:00;
 Fact = MovedToLocation (Id 2UL,Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 02:30:00;
 Fact = ProcessingStarted (Id 2UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 03:30:00;
 Fact = ProcessingEnded (Id 2UL,Id 1UL);}' generated 1 commands
Event '{Time = 12.06.2017 03:30:00;
 Fact = EntityEnteredInWaitlist (Id 2UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = ProcessingEnded (Id 1UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = CapacityReleased (Id 1UL,Id 2UL,FiniteCapacity 1u);}' generated 1 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = EntityAnnihilated (Id 1UL);}' generated 0 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = EntitySelectedFromWaitlist (Id 2UL,Id 2UL);}' generated 0 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = CapacityBlocked (Id 2UL,Id 2UL,FiniteCapacity 1u);}' generated 0 commands
Event '{Time = 12.06.2017 04:00:00;
 Fact = ChangeoverStarted (Id 2UL,Id 2UL,00:30:00);}' generated 1 commands
Event '{Time = 12.06.2017 04:30:00;
 Fact = ChangeoverEnded (Id 2UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 04:30:00;
 Fact = MovedToLocation (Id 2UL,Id 2UL);}' generated 0 commands
Event '{Time = 12.06.2017 04:30:00;
 Fact = CapacityReleased (Id 2UL,Id 1UL,FiniteCapacity 1u);}' generated 1 commands
Event '{Time = 12.06.2017 04:30:00;
 Fact = ProcessingStarted (Id 2UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 06:30:00;
 Fact = ProcessingEnded (Id 2UL,Id 2UL);}' generated 1 commands
Event '{Time = 12.06.2017 06:30:00;
 Fact = CapacityReleased (Id 2UL,Id 2UL,FiniteCapacity 1u);}' generated 1 commands
Event '{Time = 12.06.2017 06:30:00;
 Fact = EntityAnnihilated (Id 2UL);}' generated 0 commands
Simulaten terminated: Schedule is empty




********************)


