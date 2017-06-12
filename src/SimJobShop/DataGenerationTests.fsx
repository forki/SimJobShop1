open System
#load "Common.fs"
open SimJobShop.Common
#load "JobShopData.fs"
open SimJobShop.JobShopData
#load "JobShopDataGeneration.fs"
open SimJobShop.JobShopDataGeneration



(*
Preis pro Maschinen-Stunde

Amortisationskosten von Maschinen!
Was ist wenn ich eine Maschine nur 1 mal brauchen?
Preis einer Maschine ist 
*)


let p = 
    { MachineCount = 10  // 20, 100
      MinTaskCount = 5   // 8, 40
      MaxTaskCount = 8   // 15, 75
      MinProcessingTime = TimeSpan.FromMinutes(20.0)
      MaxProcessingTime = TimeSpan.FromMinutes(60.0)
      MinCapacityNeeded = 1
      MaxCapacityNeeded = 1
      MinPrice = 1.0
      MaxPrice = 10000.0
      MinUnitsPerYear = 100
      MaxUnitsPerYear = 1000
      ProductCount = 20  // 120
      JobCount = 1000 }

let data = generateJobShopData 1 p
JobShopData.writeDataToFiles """C:\Temp\test""" data
