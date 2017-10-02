module YoloDev.DependenciesIO.Project

open System
open NuGet.Versioning


module internal Lazy =
  let force (l: Lazy<'a>) = l.Force ()

module internal PlatformHelpers =
  open System.Reflection
  open System.IO
  open System.Runtime.InteropServices

  module private Unix =
    [<DllImport("libc", EntryPoint = "realpath", CharSet = CharSet.Ansi, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr realpath'(string path, IntPtr buffer)

    [<DllImport("libc", EntryPoint = "free", ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)>]
    extern void free(IntPtr ptr)

    let inline realpath path buffer = realpath' (path, buffer)

  let isMono = Type.GetType "Mono.Runtime" |> isNull
  let isWindows = Path.DirectorySeparatorChar = '\\'
  let searchPaths = lazy (
    let path = Environment.GetEnvironmentVariable "PATH"
    match path with
    | null -> []
    | path ->
      path.Split Path.PathSeparator
      |> Seq.map (fun p -> p.Trim '"')
      |> List.ofSeq )
  
  let realPath p =
    match isWindows with
    | true  -> raise <| PlatformNotSupportedException "realPath can only be called on Unix."
    | false ->
      let ptr = Unix.realpath p IntPtr.Zero
      let result = Marshal.PtrToStringAnsi ptr // uses UTF8 on Unix
      Unix.free ptr

      result

  let monoRuntimePath = lazy (
    match isWindows with
    | true  -> None
    | false ->
      let searchPaths = Lazy.force searchPaths
      searchPaths
      |> Seq.map (fun p -> Path.Combine [| p; "mono" |])
      |> Seq.filter File.Exists
      |> Seq.tryHead
      |> Option.map realPath )
  
  let monoLibDirPath = lazy (
    match isWindows with
    | true  -> None
    | false ->
      let defaultMonoLibPath = "/usr/lib/mono"
      match Directory.Exists defaultMonoLibPath with
      | true  -> Some defaultMonoLibPath
      | false ->
        
        // The normal Unix path doesn't exist, so we'll fallback to finding Mono using the
        // runtime location. This is the likely situation on macOS.
        Lazy.force monoRuntimePath
        |> Option.bind (fun monoRuntimePath ->
          let monoDirPath = Path.GetDirectoryName monoRuntimePath
          let monoLibDirPath = Path.Combine [| monoDirPath; ".."; "lib"; "mono" |] |> Path.GetFullPath

          match Directory.Exists monoLibDirPath with
          | false -> None
          | true  -> Some monoLibDirPath ))
  
  let monoMSBuildDirPath = lazy (
    match isWindows with
    | true  -> None
    | false ->
      Lazy.force monoLibDirPath
      |> Option.bind (fun monoLibDirPath ->
        let monoMSBuildDirPath = Path.Combine [| monoLibDirPath; "msbuild" |] |> Path.GetFullPath

        match Directory.Exists monoMSBuildDirPath with
        | false -> None
        | true  -> Some monoMSBuildDirPath ))
  let monoXBuildDirPath = lazy (
    match isWindows with
    | true  -> None
    | false ->
      let defaultMonoXBuildDirPath = "/usr/lib/mono/xbuild"
      match Directory.Exists defaultMonoXBuildDirPath with
      | true  -> Some defaultMonoXBuildDirPath
      | false ->
        Lazy.force monoLibDirPath
        |> Option.bind (fun monoLibDirPath ->
          let monoXBuildDirPath = Path.Combine [| monoLibDirPath; "xbuild" |] |> Path.GetFullPath
          match Directory.Exists monoXBuildDirPath with
          | false -> None
          | true  -> Some monoXBuildDirPath ))
    
  let monoXBuildFrameworksDirPath = lazy (
    match isWindows with
    | true  -> None
    | false ->
      let defaultMonoXBuildFrameworksDirPath = "/usr/lib/mono/xbuild-frameworks"
      match Directory.Exists defaultMonoXBuildFrameworksDirPath with
      | true  -> Some defaultMonoXBuildFrameworksDirPath
      | false ->
        Lazy.force monoLibDirPath
        |> Option.bind (fun monoLibDirPath ->
          let monoXBuildFrameworksDirPath = Path.Combine [| monoLibDirPath; "xbuild-frameworks" |] |> Path.GetFullPath
          match Directory.Exists monoXBuildFrameworksDirPath with
          | false -> None
          | true  -> Some monoXBuildFrameworksDirPath ))
  
  open System.Diagnostics
  let private getBasePathFromInfo dotnet =
    let info = ProcessStartInfo dotnet
    info.Arguments <- "--info"
    info.UseShellExecute <- false
    info.RedirectStandardOutput <- true
    info.CreateNoWindow <- true

    use proc = Process.Start info
    let out = proc.StandardOutput.ReadToEnd ()
    
    out.Split [| '\n' |]
    |> Seq.map (fun s -> (s.Trim '\r').Trim ())
    |> Seq.filter (fun s -> s.StartsWith "Base Path:")
    |> Seq.map (fun s -> (s.Substring "Base Path:".Length).Trim ())
    |> Seq.filter Directory.Exists
    |> Seq.tryHead

  let dotnetCliSdkDirPath = lazy (
    let searchPaths = Lazy.force searchPaths
    searchPaths
    |> Seq.map (fun p -> Path.Combine [| p; "dotnet" |])
    |> Seq.filter File.Exists
    |> Seq.choose getBasePathFromInfo
    |> Seq.tryHead
    |> Option.map realPath )

module PropertyConverter =
  open System

  let toVersionRange (v: string) =
    match VersionRange.TryParse v with
    | true, v -> Some v
    | _       -> None
  
  let toBoolean (defaultVal: bool) (v: string) =
    match v with
    | v when String.IsNullOrWhiteSpace v -> defaultVal
    | v ->
      try Convert.ToBoolean v
      with :? FormatException -> defaultVal

type Location = {
  line: int
  column: int }

type MSBuildDiagnosticsMessage = {
  logLevel: string
  fileName: string
  text: string
  startLocation: Location
  endLocation: Location }

type MSBuildOptions = {
  MSBuildSDKsPath: string option
  MSBuildExtensionsPath: string option
  TargetFrameworkRootPath: string option }

module MSBuildOptions =
  let defaultOptions = { 
    MSBuildSDKsPath = None
    MSBuildExtensionsPath = None
    TargetFrameworkRootPath = None }

type PackageDependency = {
  name: string
  version: VersionRange option }

type PackageReference = {
  dependency: PackageDependency
  isImplicitlyDefined: bool }

type ProjectFileInfo = {
  name: string
  packageReferences: PackageReference list }

[<RequireQualifiedAccess>]
type LogLevel =
| Trace
| Debug
| Info
| Warn
| Error
| Fatal

module LogLevel =
  let private allowedForTrace = Set.ofList [ LogLevel.Trace; LogLevel.Debug; LogLevel.Info; LogLevel.Warn; LogLevel.Error; LogLevel.Fatal ]
  let private allowedForDebug = Set.remove LogLevel.Trace allowedForTrace
  let private allowedForInfo  = Set.remove LogLevel.Debug allowedForDebug
  let private allowedForWarn  = Set.remove LogLevel.Info allowedForInfo
  let private allowedForError = Set.remove LogLevel.Warn allowedForWarn
  let private allowedForFatal = Set.remove LogLevel.Error allowedForError

  let allowedFor l =
    match l with
    | LogLevel.Trace -> allowedForTrace
    | LogLevel.Debug -> allowedForDebug
    | LogLevel.Info  -> allowedForInfo
    | LogLevel.Warn  -> allowedForWarn
    | LogLevel.Error -> allowedForError
    | LogLevel.Fatal -> allowedForFatal

type Logger = internal {
  level: LogLevel
  log: LogLevel -> string -> unit } with
  
  member x.logf level fmt =
    let allowed = LogLevel.allowedFor x.level
    match Set.contains level allowed with
    | false -> Printf.ksprintf ignore fmt
    | true  -> Printf.ksprintf (x.log level) fmt
  
  member x.tracef fmt = x.logf LogLevel.Trace fmt
  member x.debugf fmt = x.logf LogLevel.Debug fmt
  member x.infof  fmt = x.logf LogLevel.Info  fmt
  member x.warnf  fmt = x.logf LogLevel.Warn  fmt
  member x.errorf fmt = x.logf LogLevel.Error fmt
  member x.fatalf fmt = x.logf LogLevel.Fatal fmt

module Logger =
  let empty = { level = LogLevel.Fatal; log = fun _ -> ignore }
  
  let info log = { level = LogLevel.Info; log = log }

type MSBuildEnvironmentKind = Unknown | VisualStudio | Mono | DotnetCli

type MSBuildEnvironment = {
  kind: MSBuildEnvironmentKind
  msbuildExePath: string option
  msbuildExtensionsPath: string option
  targetFrameworkRootPath: string option
  sdksPath: string option
  toolsetPath: string option }

module MSBuildEnvironment =
  open System.Reflection
  open System.IO

  module MSBuildEnvironment =
    let withMsbuildExePath p e = { e with msbuildExePath = Some p }
    let withExtensionsPath p e = { e with msbuildExtensionsPath = Some p }
    let withTargetFrameworkRootPath p e = { e with targetFrameworkRootPath = Some p }
    let withSdksPath p e = { e with sdksPath = Some p }
    let withToolsetPath p e = { e with toolsetPath = Some p }
  
  let defaultWithKind kind = {
    kind = kind
    msbuildExePath = None
    msbuildExtensionsPath = None
    targetFrameworkRootPath = None
    sdksPath = None
    toolsetPath = None }
  
  let msBuildAssembly = Assembly.Load (AssemblyName "Microsoft.Build")
  
  let canInitializeVisualStudioBuildEnvironment =
    match PlatformHelpers.isWindows with
    | false -> false
    | true  ->
      // Call Microsoft.Build.Shared.BuildEnvironmentHelper.Initialze(...), which attempts to compute a build environment..
      let buildEnvType = msBuildAssembly.GetType "Microsoft.Build.Shared.BuildEnvironmentHelper"
      let initializeMethod = buildEnvType.GetMethod ("Initialize", BindingFlags.NonPublic ||| BindingFlags.Static)
      let env = initializeMethod.Invoke (null, null)

      match env with
      | null -> false
      | _    ->
        let buildEnvType = msBuildAssembly.GetType "Microsoft.Build.Shared.BuildEnvironment"
        let prop = buildEnvType.GetProperty ("Mode", BindingFlags.NonPublic ||| BindingFlags.Instance)
        let mode = prop.GetMethod.Invoke (env, null)
        match mode with
        | null -> false
        | mode when string mode = "VisualStudio" -> true
        | _ -> false
  
  let trySetMSBuildExtensionsPathToXBuild env =
    match PlatformHelpers.isMono with
    | false -> env
    | true  ->
      let monoXBuildDirPath = Lazy.force PlatformHelpers.monoXBuildDirPath
      match monoXBuildDirPath with
      | None   -> env
      | Some monoXBuildDirPath ->
        let monoXBuild15DirPath = Path.Combine [| monoXBuildDirPath; "15.0" |]
        match Directory.Exists monoXBuild15DirPath with
        | false -> env
        | true  -> MSBuildEnvironment.withExtensionsPath monoXBuild15DirPath env
  
  let trySetTargetFrameworkRootPathToXBuildFrameworks env =
    match PlatformHelpers.isMono with
    | false -> env
    | true  ->
      match Lazy.force PlatformHelpers.monoXBuildFrameworksDirPath with
      | None -> env
      | Some monoMSBuildXBuildFrameworksDirPath ->
        MSBuildEnvironment.withTargetFrameworkRootPath monoMSBuildXBuildFrameworksDirPath env

  let tryWithMonoMSBuild logger =
    match PlatformHelpers.isMono with
    | false -> None
    | true  ->
      match Lazy.force PlatformHelpers.monoMSBuildDirPath with
      | None -> None
      | Some monoMSBuildDirPath ->
        let monoMSBuildBinDirPath = Path.Combine [| monoMSBuildDirPath; "15.0"; "bin" |]
        let msbuildExePath = Path.Combine [| monoMSBuildBinDirPath; "MSBuild.dll" |]

        match File.Exists msbuildExePath with
        | false -> None
        | true  ->

          defaultWithKind Mono
          |> MSBuildEnvironment.withMsbuildExePath msbuildExePath
          |> trySetMSBuildExtensionsPathToXBuild
          |> trySetTargetFrameworkRootPathToXBuildFrameworks
          |> Some
  
  let tryWithDotnetMSBuild logger =
    Lazy.force PlatformHelpers.dotnetCliSdkDirPath
    |> Option.map (fun path ->
      defaultWithKind DotnetCli
      |> MSBuildEnvironment.withMsbuildExePath (Path.Combine [| path; "MSBuild.dll" |])
      |> MSBuildEnvironment.withSdksPath (Path.Combine [| path; "Sdks" |])
      |> MSBuildEnvironment.withExtensionsPath path
      |> MSBuildEnvironment.withToolsetPath path )

  let get (logger: Logger) =
    // If MSBuild can locate VS 2017 and set up a build environment, we don't need to do anything.
    // MSBuild will take care of itself.
    let env =
      match canInitializeVisualStudioBuildEnvironment with
      | true -> Some <| defaultWithKind VisualStudio
      | false ->
        tryWithMonoMSBuild logger
        |> Option.orElseWith (fun () -> tryWithDotnetMSBuild logger)
    
    match env with
    | None -> failwithf "MSBuild environment could not be initialized."
    | Some env -> env

module ProjectFileInfo =
  open System.IO
  let private addPropertyIfNeeded (logger: Logger) (name: string) (userOptionValue: string option) (environmentValue: string option) props =
    match userOptionValue with
    | Some v -> Map.add name v props
    | None   ->
      match environmentValue with
      | Some v -> Map.add name v props
      | None   ->
        match Map.tryFind name props with
        | None -> props
        | Some v ->
          logger.debugf "Using %s: %s" name v
          props

  let private getGlobalProperties (env: MSBuildEnvironment) (options: MSBuildOptions) solutionDirectory logger =
    Map.ofList [
      "DesignTimeBuild", "true"
      "BuildProjectReferences", "false"
      "_ResolveReferenceDependencies", "true"
      "SolutionDir", sprintf "%s%c" solutionDirectory Path.DirectorySeparatorChar

      // This properties allow the design-time build to handle the Compile target without actually invoking the compiler.
      // See https://github.com/dotnet/roslyn/pull/4604 for details.
      "ProvideCommandLineInvocation", "true"
      "SkipCompilerExecution", "true" ]
    
    |> addPropertyIfNeeded logger "MSBuildExtensionsPath" options.MSBuildExtensionsPath env.msbuildExtensionsPath
    |> addPropertyIfNeeded logger "MSBuildSDKsPath" options.MSBuildSDKsPath env.sdksPath
    |> addPropertyIfNeeded logger "TargetFrameworkRootPath" options.TargetFrameworkRootPath env.targetFrameworkRootPath
    

  open Microsoft.Build.Execution
  open Microsoft.Build.Evaluation
  let private getPackageReferences =
      Seq.map <| fun (item: ProjectItemInstance) ->
        let name = item.EvaluatedInclude
        let versionValue = item.GetMetadataValue "Version" |> PropertyConverter.toVersionRange
        let dep = { name = name; version = versionValue }
        
        let isImplicitlyDefined = item.GetMetadataValue "IsImplicitlyDefined" |> PropertyConverter.toBoolean false
        { dependency = dep; isImplicitlyDefined = isImplicitlyDefined }
    >> Seq.distinct 

  let private (<|>) s1 s2 =
    if String.IsNullOrWhiteSpace s1 then s2 else s1
  
  let tmpEnv name value =
    let old = Environment.GetEnvironmentVariable name
    match value with
    | Some v -> 
      Environment.SetEnvironmentVariable (name, v)
      { new IDisposable with
        member x.Dispose () =
          Environment.SetEnvironmentVariable (name, old) }
    | None   -> 
      { new IDisposable with
        member x.Dispose () = () }

  let load env (filePath: string) (solutionDirectory: string) (logger: Logger) (options: MSBuildOptions option) =
    let options = defaultArg options MSBuildOptions.defaultOptions

    let globalProperties = getGlobalProperties env options solutionDirectory logger
    let MSBuildSDKsPath = "MSBuildSDKsPath"
    use tmp1 = tmpEnv "MSBuildSDKsPath" (Map.tryFind MSBuildSDKsPath globalProperties)
    use tmp2 = tmpEnv "MSBUILD_EXE_PATH" env.msbuildExePath

    use collection = new ProjectCollection (globalProperties)
    let toolsVersion = collection.DefaultToolsVersion
    logger.infof "Using tools version %s" toolsVersion
    
    (* match env.toolsetPath with
    | None -> ()
    | Some path ->
      collection.RemoveAllToolsets ()
      collection.SetGlobalProperty ("RoslynTargetsPath", Path.Combine [| path; "Roslyn" |])
      collection.AddToolset <|
        Toolset (
          toolsVersion,
          path,
          collection,
          "" ) *)


    // Evaluate the MSBuild project
    let project = collection.LoadProject (filePath, toolsVersion)
    let projectInstance = project.CreateProjectInstance ()

    let name = 
          projectInstance.GetPropertyValue "ProjectName"
      <|> projectInstance.GetPropertyValue "MSBuildProjectName"
    let packageReferences = 
      projectInstance.GetItems "PackageReference" 
      |> getPackageReferences 
      |> List.ofSeq

    { name = name
      packageReferences = packageReferences }