#load "Common.fs"

open SimJobShop.Common

// Repository test
let makeItem id = sprintf "item [%A]" id

let log (id, repo) = 
    printfn "item inserted with id = %A" id
    repo

let r1 = 
    Repository.createDefault<string>()
    |> Repository.insert makeItem
    |> log
    |> Repository.insert makeItem
    |> log
    |> Repository.insert makeItem
    |> log

let idSeq = Map.toSeq r1.Items |> Seq.map (fun (key, value) -> key)
let id1 = Seq.head idSeq
let r2R = Repository.get id1 r1
let r3R = Repository.update id1 "foo bar" r1
let r4R = Repository.delete id1 r1
