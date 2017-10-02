// Learn more about F# at http://fsharp.org

open FSharp.Control
open System.IO
open YoloDev.DependenciesIO.Project
open YoloDev.DependenciesIO.Solution
open YoloDev.DependenciesIO.NuGet
open YoloDev.DependenciesIO.Json
open YoloDev.DependenciesIO.Json.Operators

[<RequireQualifiedAccess>]
module PackageReference =
  let isExplicit (p: PackageReference) =
    not p.isImplicitlyDefined

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

[<Struct>]
type Dependencies = Dependencies of Dependency list with

  static member ToJson (Dependencies deps) =
    Json.write "dependencies" deps

let getDependencies provider (proj: SolutionProject) = asyncSeq {
  let deps =
    proj.dependencies
    |> Seq.filter PackageReference.isExplicit

  for dep in deps do
    let name = dep.dependency.name
    let path = proj.path
    let installed = 
      dep.dependency.version
      |> Option.map string
      |> Option.defaultValue "*"
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
          path = path
          source = source } }

let getAllDependencies rootDir solution (projects: AsyncSeq<SolutionProject>) = asyncSeq {
  use provider = NuGet.create <| Path.Combine [| rootDir; solution.dir |]
  for project in projects do
    yield! getDependencies provider project }

[<EntryPoint>]
let main argv =
  let givenPath = (Array.head argv).TrimStart [| '/' |]
  let rootDir = "/repo"
  let relPath = (Array.head argv).TrimStart [| '/' |]
  let absPath = Path.Combine [| rootDir; relPath |]
  let solution, projects = Solution.load relPath absPath
  let deps = getAllDependencies rootDir solution projects
  
  deps
  |> AsyncSeq.toListAsync
  |> Async.RunSynchronously
  |> Dependencies
  |> Json.serialize
  |> Json.formatWith Format.Compact
  |> printfn "BEGIN_DEPENDENCIES_SCHEMA_OUTPUT>%s<END_DEPENDENCIES_SCHEMA_OUTPUT"
  
  0