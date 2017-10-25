[<AutoOpen>]
module DependenciesIO.MsBuild

open Json
open Json.Operators

module internal Async =
  let inline unit x = async.Return x

  let inline bind f a = async.Bind (a, f)

  let inline map f = bind (f >> unit)

module MsBuildDependencyGraph =
  type ProjectInfo =
    { name: string
      path: string
      sources: string list }
  
    static member FromJson (_: ProjectInfo) =
          fun (n: string) (p: string) (s: Map<string, Map<string, string>>) ->
            { name = n
              path = p
              sources = s |> Map.toList |> List.map fst }
      <!> Json.read "projectName"
      <*> Json.read "projectPath"
      <*> Json.read "sources"
  
  type DependencyRecord =
    { version: string
      autoReferenced: bool }
    
    static member FromJson (_: DependencyRecord) =
          fun (version: string) (autoReferenced: bool) ->
            { version = version
              autoReferenced = autoReferenced }
      <!> Json.read "version"
      <*> Json.readOrDefault "autoReferenced" false

  type Dependency =
    { name: string
      version: string
      autoReferenced: bool }
  
  [<RequireQualifiedAccess>]
  module Dependency =
    let ofRecord (name: string, record: DependencyRecord) =
      { name = name
        version = record.version
        autoReferenced = record.autoReferenced }
    
    let isAutoReferenced dep = dep.autoReferenced
  
  type FrameworkRecord =
    { dependencies: Dependency list }

    static member FromJson (_: FrameworkRecord) =
          fun (dependencyRecords: Map<string, DependencyRecord>) ->
            { dependencies = dependencyRecords |> Map.toList |> List.map Dependency.ofRecord }
      <!> Json.read "dependencies"
  
  [<RequireQualifiedAccess>]
  module FrameworkRecord =
    let dependencies framework = framework.dependencies
  
  type Project =
    { info: ProjectInfo
      dependencies: Map<string, Dependency list> }
    
    static member FromJson (_: Project) =
          fun (info: ProjectInfo) (frameworks: Map<string, FrameworkRecord>) ->
            { info = info
              dependencies = frameworks |> Map.map (fun _ -> FrameworkRecord.dependencies) }
      <!> Json.read "restore"
      <*> Json.read "frameworks"
  
  // TODO: Given the monadic nature of the JSON parsing
  // it should be totally possible to model different graph
  // format verisons in a simple way.
  type Graph =
    { format: int64
      projects: Project list }
    
    static member FromJson (_: Graph) =
          fun (format: int64) (projects: Map<string, Project>) ->
            { format = format
              projects = projects |> Map.toList |> List.map snd }
      <!> Json.read "format"
      <*> Json.read "projects"

open MsBuildDependencyGraph

[<RequireQualifiedAccess>]
module MsBuild =
  let getDependencyGraph dir projOrSln : Async<Graph> =
    MsBuild.getResourceGraphJson dir projOrSln
    |> Async.map Json.parse
    |> Async.map Json.deserialize