module DependenciesIO.Actor.Program

open System.IO

[<EntryPoint>]
let main argv =
  let givenPath = (Array.head argv).TrimStart [| '/' |]
  let rootDir = "/repo"
  let relPath = (Array.head argv).TrimStart [| '/' |]
  let absPath = Path.Combine [| rootDir; relPath |]
  //let solution, projects = Solution.load relPath absPath
  0