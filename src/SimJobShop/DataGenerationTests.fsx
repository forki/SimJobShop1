open System
#load "Common.fs"
open SimJobShop.Common
#load "JobShopData.fs"
open SimJobShop.JobShopData
#load "JobShopDataGeneration.fs"
open SimJobShop.JobShopDataGeneration

let p = 
    { MachineCount = 10  //20
      MinTaskCount = 5
      MaxTaskCount = 8 //15
      MinProcessingTime = TimeSpan.FromMinutes(20.0)
      MaxProcessingTime = TimeSpan.FromMinutes(30.0)
      MinCapacityNeeded = 1
      MaxCapacityNeeded = 1
      MinPrice = 1.0
      MaxPrice = 10.0
      MinUnitsPerYear = 100
      MaxUnitsPerYear = 1000
      ProductCount = 20  //120
      JobCount = 1000 }

let data = generateJobShopData 1 p
JobShopData.writeDataToFiles """C:\Temp\test""" data
