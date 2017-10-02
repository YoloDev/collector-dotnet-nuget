module DependenciesIO.Solution

open FSharp.Control
open Microsoft.Build.Construction
open Project

type Solution = {
  name: string
  path: string
  dir: string }

type SolutionProject = {
  name: string
  path: string
  dependencies: PackageReference list
  solution: Solution }

[<RequireQualifiedAccess>]
module Solution =
  open System.IO

  let load relPath absPath = 
    let relDir = Path.GetDirectoryName relPath
    let slnDir = Path.GetDirectoryName absPath
    let logger = Logger.info (printfn "[%A] %s")
    let env = MSBuildEnvironment.get logger
    let sln = SolutionFile.Parse absPath
    let solution = 
      { name = Path.GetFileNameWithoutExtension relPath
        path = relPath
        dir = relDir }

    solution, asyncSeq {
      for project in sln.ProjectsInOrder do
        if project.RelativePath.EndsWith "proj" then
          let fromSlnPath = project.RelativePath.Replace ('\\', Path.DirectorySeparatorChar)
          let relPath = Path.Combine [| relDir; fromSlnPath |]
          let absPath = Path.Combine [| slnDir; fromSlnPath |]
          let proj = ProjectFileInfo.load env absPath slnDir logger None
          let name = proj.name
          
          yield 
            { name = name 
              path = relPath 
              dependencies = proj.packageReferences
              solution = solution } }