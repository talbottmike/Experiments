module HashInfo
open System
open System.IO

let tryGetEnv =
  System.Environment.GetEnvironmentVariable
  >> function
  | null | "" -> None
  | x -> Some x

let hash = tryGetEnv "webpack_hash"

let withHash prefix suffix =
  hash
  |> Option.map (fun x -> sprintf "%s.%s.%s" prefix x suffix)
  |> Option.defaultValue (sprintf "%s.%s" prefix suffix)