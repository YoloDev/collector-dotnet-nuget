module DependenciesIO.NuGet

open System
open FSharp.Control
open NuGet.Common
open NuGet.Protocol
open NuGet.Protocol.Core.Types
open NuGet.Configuration
open NuGet.Versioning

type Provider = private {
  repos: SourceRepository list
  cache: SourceCacheContext
  logger: ILogger } with

  interface IDisposable with
    member x.Dispose () =
      x.cache.Dispose ()

type Versions = {
  source: string
  versions: NuGetVersion list }

[<RequireQualifiedAccess>]
module NuGet =
  let create (dir: string) =
    let settings = Settings.LoadDefaultSettings dir
    let provider = SourceRepositoryProvider (settings, Repository.Provider.GetCoreV3 ())
    let repos = 
      provider.PackageSourceProvider.LoadPackageSources ()
      |> Seq.map provider.CreateRepository
      |> Seq.toList
    

    { repos = repos
      cache = new SourceCacheContext ()
      logger = NullLogger () }

  let rec private getAvailableVersions' (repos: SourceRepository list) cache logger packageId =
    match repos with
    | []            -> async { return None }
    | repo :: repos -> async {
      let! byId = repo.GetResourceAsync<FindPackageByIdResource> () |> Async.AwaitTask
      match Option.ofObj byId with
      | None      -> return! getAvailableVersions' repos cache logger packageId
      | Some byId ->
        let! token = Async.CancellationToken
        let! versions = byId.GetAllVersionsAsync (packageId, cache, logger, token) |> Async.AwaitTask
        let versions = versions |> List.ofSeq

        match versions with
        | [] -> return! getAvailableVersions' repos cache logger packageId
        | _  -> return { source = repo.PackageSource.Source; versions = versions } |> Some }
        
  let getAvailableVersions (provider: Provider) (packageId: string) =
    getAvailableVersions' provider.repos provider.cache provider.logger packageId
