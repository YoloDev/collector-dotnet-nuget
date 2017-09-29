// Learn more about F# at http://fsharp.org

open System

module internal Option =
  let ifType<'a> (a: obj) =
    match a with
    | :? 'a as a -> Some a
    | _          -> None

module internal OptionOperators =
  let (>>=) ma f = Option.bind f ma
  let (>==) ma f = Option.map f ma
  let (===) ma b =
    match ma with
    | Some a when a = b -> Some ()
    | _                 -> None

open OptionOperators

module internal AsyncResultOperators =
  let private asyncBind (f: 'a -> Async<Result<'c, 'b>>) (xs: 'a seq) =
    let rec next (f: 'a -> Async<Result<'c, 'b>>) (xs: 'a list) (acc: 'c list) =
      match xs with
      | []    -> async { return Ok (Seq.rev acc) }
      | x::xs -> async {
        let! y = f x
        match y with
        | Error e -> return Error e
        | Ok y    -> return! next f xs (y :: acc) }
    
    next f (xs |> Seq.toList) []
  
  let private asyncMap (f: 'a -> 'c) = asyncBind (f >> Ok >> async.Return)

  let (|?>) (m: Async<Result<'a, 'b>>) (f: 'b -> 'c) = async {
    let! x = m
    match x with
    | Ok x    -> return Ok x
    | Error e -> return Error (f e) }
  
  let (|->) (m: Async<Result<'a, 'b>>) (f: 'a -> 'c) = async {
    let! x = m
    match x with
    | Error e -> return Error e
    | Ok x    -> return Ok (f x) }
  
  let (|->>) (m: Async<Result<'a, 'b>>) (f: 'a -> Async<Result<'c, 'b>>) = async {
    let! x = m
    match x with
    | Error e -> return Error e
    | Ok x    -> return! f x
  }

  let (||->>) (m: Async<Result<'a seq, 'b>>) (f: 'a -> Async<Result<'c, 'b>>) = async {
    let! x = m
    match x with
    | Error e -> return Error e
    | Ok xs   -> return! asyncBind f xs }
  
  let (||-|>) (m: Async<Result<'a seq, 'b>>) (f: 'a -> 'c seq) = async {
    let! x = m
    match x with
    | Error e -> return Error e
    | Ok xs   -> 
      let! ys = asyncMap f xs
      match ys with
      | Error e -> return Error e
      | Ok ys   -> return Seq.collect id ys |> Ok }
  
  let (||->) (m: Async<Result<'a seq, 'b>>) (f: 'a -> 'c) = async {
    let! x = m
    match x with
    | Error e -> return Error e
    | Ok xs   -> return Seq.map f xs |> Ok }

open AsyncResultOperators

module internal Proc =
  open System.Diagnostics

  let exec dir cmd args = async {
    let! cancellationToken = Async.CancellationToken
    let args =
      args
      |> List.map (sprintf "\"%s\"")
      |> String.concat " "
    
    printfn "run %s %s in dir '%s'" cmd args dir

    use proc = 
      ProcessStartInfo (cmd, args, WorkingDirectory = dir)
      |> Process.Start
    
    let tcs = System.Threading.Tasks.TaskCompletionSource<int> ()
    proc.EnableRaisingEvents <- true
    proc.Exited.AddHandler (fun _ _ -> tcs.TrySetResult proc.ExitCode |> ignore)
    if cancellationToken.CanBeCanceled then
      cancellationToken.Register (fun () -> tcs.TrySetCanceled () |> ignore) |> ignore
    
    let! status = Async.AwaitTask tcs.Task
    match status with
    | 0 -> return Ok ()
    | _ -> return Error status }

type internal FileContent = {
  path: string
  content: string }

type internal InstalledDep = {
  owner: string
  name: string
  version: string }

type internal ResolvedDep = {
  installed: InstalledDep
  available: NuGet.Versioning.NuGetVersion list
}

module internal File =
  open System.IO

  let find pattern dir = async {
    let files = 
      Directory.EnumerateFiles (dir, pattern, SearchOption.AllDirectories)
      |> Seq.toList
    return 
      files
      |> List.toSeq
      |> Ok }
  
  let read file = async {
    let! c = Async.CancellationToken
    let! str = File.ReadAllTextAsync (file, c) |> Async.AwaitTask
    return Ok { path = file; content = str } }

module internal Json =
  open Newtonsoft.Json.Linq
  type Token = JToken

  let parse str = 
    try Some (JObject.Parse str)
    with e -> None
  
  let prop name (obj: JObject) =
    obj.Property name
    |> Option.ofObj
    >>= (fun p -> Option.ofObj p.Value)
  
  let asObj (tok: JToken) =
    match tok with
    | :? JObject as o -> Some o
    | _               -> None
  
  let private getValue (tok: JToken) =
    match tok with
    | :? JValue as v -> Some v.Value
    | _              -> None
  
  let asString (tok: JToken) =
    tok
    |> getValue
    >>= Option.ifType<string>
  
  let props (obj: JObject) = 
    obj.Properties ()
    |> Seq.choose (fun p -> 
      Option.ofObj p.Value
      >== (fun v -> p.Name, v))

module internal DepJson =
  open Newtonsoft.Json.Linq

  let private obj (props: JProperty list) = JObject (props |> Seq.cast<obj> |> Array.ofSeq) :> JToken
  let private prop name (value: JToken) = JProperty (name, value)
  let private str (s: string) = JValue s :> JToken
  let arr (values: JToken list) = JArray (values |> Seq.cast<obj> |> Array.ofSeq) :> JToken

  let toJson (dep: ResolvedDep) =
    let available =
      dep.available
      |> List.map (fun v -> obj [ prop "version" (str v.OriginalVersion) ])
      |> arr

    let o = 
      obj [
        prop "name" (str dep.installed.name)
        prop "installed" (obj [prop "version" (str dep.installed.version)])
        prop "available" available
        prop "path" (str dep.installed.owner)
        prop "source" (str "nuget") ]

    o

module internal NuGet =
  open NuGet.Common
  open NuGet.Protocol
  open NuGet.Protocol.Core.Types
  open NuGet.Configuration

  let provider (path: string) =
    let settings = Settings.LoadDefaultSettings path
    SourceRepositoryProvider (settings, Repository.Provider.GetCoreV3 ())
  
  let getVersions (res: FindPackageByIdResource) cacheContext logger (packageId: string) = async {
    let! token = Async.CancellationToken
    let! versions = res.GetAllVersionsAsync (packageId, cacheContext, logger, token) |> Async.AwaitTask
    let versions = versions |> List.ofSeq
    match versions with
    | [] -> return None
    | _  -> return Some versions }
  
  let getAvailableVersions (provider: SourceRepositoryProvider) (packageId: string) =
    let packageSources = provider.PackageSourceProvider.LoadPackageSources () |> List.ofSeq
    let rec next cacheContext logger sources = async {
      match sources with
      | [] -> return Error (sprintf "Package '%s' not found in any of the registered sources" packageId)
      | source::sources ->
        let repo = provider.CreateRepository source
        let! byId = repo.GetResourceAsync<FindPackageByIdResource> () |> Async.AwaitTask
        match Option.ofObj byId with
        | None -> return! next cacheContext logger sources
        | Some byId ->
          let! token = Async.CancellationToken
          let! versions = byId.GetAllVersionsAsync (packageId, cacheContext, logger, token) |> Async.AwaitTask
          let versions = versions |> List.ofSeq
          match versions with
          | [] -> return! next cacheContext logger sources
          | _  -> return Ok versions }
    
    async {
      use cacheContext = new SourceCacheContext ()
      let logger = NullLogger ()
      return! next cacheContext logger packageSources
    }

module internal Dotnet =
  open System.IO

  let restore dir = 
    Proc.exec dir "dotnet" ["restore"]
    |?> sprintf "Restore in dir %s failed with status %d" dir
  
  let parseDep path (name: string, dep: Json.Token) =
    dep
    |> Json.asObj
    >>= Json.prop "type"
    >>= Json.asString
    === "package"
    >>= (fun () ->
      let parts = name.Split ([| '/' |]) |> List.ofArray
      let projDir = path |> Path.GetDirectoryName |> Path.GetDirectoryName
      let proj = Directory.GetFiles (projDir, "*.*proj") |> Array.tryHead
      match proj with
      | None -> None
      | Some proj -> 
        match parts with
        | [name; version] -> { name = name; version = version; owner = proj } |> Some
        | _               -> None )
  
  let parseDeps (deps: FileContent) = 
    Json.parse deps.content
    >>= Json.prop "libraries"
    >>= Json.asObj
    >== Json.props
    >== Seq.choose (parseDep deps.path)
    |> Option.defaultValue Seq.empty
  
  let resolveDep dir (installed: InstalledDep) = async {
    let provider = NuGet.provider dir
    let! versions = NuGet.getAvailableVersions provider installed.name
    match versions with
    | Error e     -> return Error e
    | Ok versions -> return { installed = installed; available = versions } |> Ok }
  
  let findDeps dir =
    restore dir
    |->> (fun () -> File.find "project.assets.json" dir)
    ||->> File.read
    ||-|> parseDeps
    ||->> resolveDep dir
    |-> (Seq.toList >> Seq.ofList)
    ||-> DepJson.toJson
    |-> Seq.toList
    |-> DepJson.arr

[<EntryPoint>]
let main argv =
  let givenPath = (Array.head argv).Trim [| '/' |]
  let depPath = System.IO.Path.Combine [| "/repo"; givenPath |]
  let async = Dotnet.findDeps depPath
  let result = Async.RunSynchronously async
  match result with
  | Error e ->
    printfn "Error: %s" e
    1
  | Ok deps ->
    printfn "BEGIN_DEPENDENCIES_SCHEMA_OUTPUT>{\"dependencies\":%s}<END_DEPENDENCIES_SCHEMA_OUTPUT" <| deps.ToString Newtonsoft.Json.Formatting.None
    0
