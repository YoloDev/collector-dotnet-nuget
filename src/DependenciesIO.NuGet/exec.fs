[<AutoOpen>]
module DependenciesIO.Exec

[<RequireQualifiedAccess>]
module Exec =
  open System.Diagnostics
  open System.Threading.Tasks

  let run dir name args =
    async {
      let! cancellationToken = Async.CancellationToken
      let tcs = TaskCompletionSource ()
      let args = String.concat " " <| List.map (sprintf "\"%s\"") args
      let pci = ProcessStartInfo (name, args, WorkingDirectory = dir)
      use proc = Process.Start pci
      proc.EnableRaisingEvents <- true
      proc.Exited.Add (tcs.TrySetResult >> ignore)
      do! Async.AwaitTask tcs.Task |> Async.Ignore
      return proc.ExitCode
    }

[<RequireQualifiedAccess>]
module MsBuild =
  open System.IO

  let run dir projOrSlnFile target props =
    let targetArg = sprintf "/t:%s" target
    let propsArgs =
      props
      |> Map.toList
      |> List.map (fun (name, value) -> sprintf "/p:%s=%s" name value)
    
    let args = ["msbuild"; projOrSlnFile; targetArg] @ propsArgs
    Exec.run dir "dotnet" args

  let getResourceGraphJson dir projOrSlnFile =
    async {
      let tempFile = Path.GetTempFileName ()
      try
        let! exitCode =
          run dir projOrSlnFile "GenerateRestoreGraphFile" <| Map.ofList
            [
              "RestoreGraphOutputPath", tempFile
            ]
        
        match exitCode with
        | 0 ->
          use fs = File.OpenRead tempFile
          use sr = new StreamReader (fs)
          return! sr.ReadToEndAsync () |> Async.AwaitTask
        
        | _ ->
          return failwithf "GenerateRestoreGraphFile exited with error code %d for project %s" exitCode projOrSlnFile

      finally
        try File.Delete tempFile with | _ -> ()
    }
