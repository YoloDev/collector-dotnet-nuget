module DependenciesIO.Collector.Program

open FSharp.Control
open System.IO
open DependenciesIO
open DependenciesIO.NuGet
open DependenciesIO.Json
open DependenciesIO.Json.Operators

[<Struct>]
type Version = Version of string with

  static member ToJson (Version x) =
    Json.write "version" x

type Dependency = {
  name: string
  installed: string
  available: string list
  path: string
  source: string } with

  static member ToJson (x: Dependency) =
          Json.write "name" x.name
       *> Json.write "installed" (Version x.installed)
       *> Json.write "available" (List.map Version x.available)
       *> Json.write "path" x.path
       // *> Json.write "repo" x.source
       *> Json.write "source" "nuget"


module Path =
  let getRelative fromDir toPath =
    if isNull fromDir then nullArg "fromDir"
    elif isNull toPath then nullArg "toPath"
    
    let isDifferentRoot = 
      let isRooted = Path.IsPathRooted fromDir && Path.IsPathRooted toPath
      if not isRooted then false
      else System.String.Compare (Path.GetPathRoot fromDir, Path.GetPathRoot toPath, true) <> 0
    
    if isDifferentRoot then toPath
    else
      let relativePath = ResizeArray ()
      let fromDirectories = fromDir.Split Path.DirectorySeparatorChar |> List.ofArray
      let toDirectories = toPath.Split Path.DirectorySeparatorChar |> List.ofArray

      // find common root
      let rec chk fromDirectories toDirectories index =
        match fromDirectories, toDirectories with
        | h1::fromDirectories, h2::toDirectories ->
          if System.String.Compare (h1, h2, true) <> 0 then index
          else chk fromDirectories toDirectories (index + 1)
        | _ -> index
      
      let lastCommonRoot = chk fromDirectories toDirectories -1
      if lastCommonRoot = -1 then toPath
      else
        // add relative folders in from path
        for x in (lastCommonRoot + 1)..(List.length fromDirectories - 1) do
          if fromDirectories |> List.item x |> String.length > 0 then
            relativePath.Add ".."
        
        // add to folders to path
        for x in (lastCommonRoot + 1)..(List.length toDirectories - 1) do
          toDirectories |> List.item x |> relativePath.Add
        
        // create relative path
        String.concat (string Path.DirectorySeparatorChar) relativePath
    

[<Struct>]
type Dependencies = Dependencies of Dependency list with

  static member ToJson (Dependencies deps) =
    Json.write "dependencies" deps

let getDependencies rootDir (project: MsBuildDependencyGraph.Project) = asyncSeq {
  let deps =
    project.dependencies
    |> Map.map (fun _ -> List.filter (MsBuildDependencyGraph.Dependency.isAutoReferenced >> not))
    |> Map.filter (fun _ -> List.isEmpty >> not)
  
  use provider = NuGet.create (Path.GetDirectoryName project.info.path)
  for framework, deps in deps |> Map.toList do
    for dep in deps do
      let name = dep.name
      let path = sprintf "%s!%s" project.info.path framework
      let installed = dep.version
      let! available = NuGet.getAvailableVersions provider name
      match available with
      | None -> ()
      | Some ({ versions = versions; source = source }) ->
        let versionStrings =
          versions
          |> List.map string

        yield
          { name = name
            installed = installed
            available = versionStrings
            path = Path.getRelative rootDir path
            source = source } }

let getAllDependencies rootDir relPath absPath = asyncSeq {
  let! graph = MsBuild.getDependencyGraph rootDir absPath
  for project in graph.projects do
    yield! getDependencies rootDir project }

[<EntryPoint>]
let main argv =
  let givenPath = (Array.head argv).TrimStart [| '/' |]
  let rootDir = "/repo"
  let relPath = (Array.head argv).TrimStart [| '/' |]
  let absPath = Path.Combine [| rootDir; relPath |]
  let deps = getAllDependencies rootDir relPath absPath
  
  printfn "start"
  deps
  |> AsyncSeq.toListAsync
  |> Async.RunSynchronously
  |> Dependencies
  |> Json.serialize
  |> Json.formatWith Format.Compact
  |> printfn "BEGIN_DEPENDENCIES_SCHEMA_OUTPUT>%s<END_DEPENDENCIES_SCHEMA_OUTPUT"
  
  0