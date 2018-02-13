// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open Fake.UserInputHelper
open Fake.Testing.XUnit2
open System
open System.IO

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "TypeShape"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Practical Generic Programming in F#"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "Practical Generic Programming in F#"

// List of author names (for NuGet package)
let authors = [ "Eirik Tsarpalis" ]

// Tags for your project (for NuGet package)
let tags = "fsharp, reflection"

// File system information
let solutionFile  = "TypeShape.sln"

// Folder to deposit deploy artifacts
let artifactsDir = __SOURCE_DIRECTORY__ @@ "artifacts"

// Pattern specifying assemblies to be tested
let testProjects = "tests/**/*.??proj"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "eiriktsarpalis"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "TypeShape"

// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/eiriktsarpalis"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title (projectName)
          Attribute.Product project
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
        | Shproj -> ()
        )
)

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs [ "bin" ; artifactsDir ; "temp" ]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

Target "Restore" (fun _ ->
    DotNetCli.Restore id
)

// --------------------------------------------------------------------------------------
// Build library & test project


Target "Build" DoNothing

let buildWithConfiguration config =
    !! solutionFile
    |> MSBuild null "Rebuild" [("Configuration", config) ; ("SourceLinkCreate", "true")]
    |> ignore  

Target "Build.Emit" (fun _ -> buildWithConfiguration "Release")
Target "Build.NoEmit" (fun _ -> buildWithConfiguration "Release-NoEmit")

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

let runTests config (proj : string) =
    if EnvironmentHelper.isWindows then
        DotNetCli.Test (fun c ->
            { c with
                Project = proj
                Configuration = config })
    else
        // work around xunit/mono issue
        let projDir = Path.GetDirectoryName proj
        let projName = Path.GetFileNameWithoutExtension proj
        let netcoreFrameworks, legacyFrameworks = 
            !! (projDir @@ "bin" @@ config @@ "*/")
            |> Seq.map Path.GetFileName
            |> Seq.toArray
            |> Array.partition 
                (fun f -> 
                    f.StartsWith "netcore" || 
                    f.StartsWith "netstandard")
            

        for framework in netcoreFrameworks do
            DotNetCli.Test (fun c ->
                { c with
                    Project = proj
                    Framework = framework
                    Configuration = config })

        for framework in legacyFrameworks do
            let assembly = projDir @@ "bin" @@ config @@ framework @@ projName + ".dll"
            !! assembly
            |> xUnit2 (fun c ->
                { c with
                    Parallel = ParallelMode.Collections
                    TimeOut = TimeSpan.FromMinutes 20. })

Target "RunTests" DoNothing

Target "RunTests.Release" (fun _ ->
    for proj in !! testProjects do
        runTests "Release" proj
)

Target "RunTests.Release-NoEmit" (fun _ ->
    for proj in !! testProjects do
        runTests "Release-NoEmit" proj
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet.Bundle" (fun _ ->
    Paket.Pack(fun p ->
        { p with
            OutputPath = artifactsDir
            Version = release.NugetVersion
            BuildPlatform = "AnyCpu"
            ReleaseNotes = toLines release.Notes})
)

Target "NuGet.ValidateSourceLink" (fun _ ->
    for nupkg in !! (artifactsDir @@ "*.nupkg") do
        DotNetCli.RunCommand
            (fun p -> { p with WorkingDir = __SOURCE_DIRECTORY__ @@ "tests" @@ "TypeShape.Tests.Core" } )
            (sprintf "sourcelink test %s" nupkg)
)

Target "NuGet.Push" (fun _ ->
    Paket.Push(fun p ->
        { p with
            WorkingDir = artifactsDir })
)

// --------------------------------------------------------------------------------------
// Generate the documentation


let fakePath = "packages" </> "build" </> "FAKE" </> "tools" </> "FAKE.exe"
let fakeStartInfo script workingDirectory args fsiargs environmentVars =
    (fun (info: System.Diagnostics.ProcessStartInfo) ->
        info.FileName <- System.IO.Path.GetFullPath fakePath
        info.Arguments <- sprintf "%s --fsiargs -d:FAKE %s \"%s\"" args fsiargs script
        info.WorkingDirectory <- workingDirectory
        let setVar k v =
            info.EnvironmentVariables.[k] <- v
        for (k, v) in environmentVars do
            setVar k v
        setVar "MSBuild" msBuildExe
        setVar "GIT" Git.CommandHelper.gitPath
        setVar "FSI" fsiPath)

/// Run the given buildscript with FAKE.exe
let executeFAKEWithOutput workingDirectory script fsiargs envArgs =
    let exitCode =
        ExecProcessWithLambdas
            (fakeStartInfo script workingDirectory "" fsiargs envArgs)
            TimeSpan.MaxValue false ignore ignore
    System.Threading.Thread.Sleep 1000
    exitCode

// Documentation
let buildDocumentationTarget fsiargs target =
    trace (sprintf "Building documentation (%s), this could take some time, please wait..." target)
    let exit = executeFAKEWithOutput "docs/tools" "generate.fsx" fsiargs ["target", target]
    if exit <> 0 then
        failwith "generating reference documentation failed"
    ()

Target "GenerateReferenceDocs" (fun _ ->
    buildDocumentationTarget "-d:RELEASE -d:REFERENCE" "Default"
)

let generateHelp' fail debug =
    let args =
        if debug then "--define:HELP"
        else "--define:RELEASE --define:HELP"
    try
        buildDocumentationTarget args "Default"
        traceImportant "Help generated"
    with
    | e when not fail ->
        traceImportant "generating help documentation failed"

let generateHelp fail =
    generateHelp' fail false

Target "GenerateHelp" (fun _ ->
    DeleteFile "docs/content/release-notes.md"
    CopyFile "docs/content/" "RELEASE_NOTES.md"
    Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    DeleteFile "docs/content/license.md"
    CopyFile "docs/content/" "LICENSE.txt"
    Rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp true
)

Target "GenerateHelpDebug" (fun _ ->
    DeleteFile "docs/content/release-notes.md"
    CopyFile "docs/content/" "RELEASE_NOTES.md"
    Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    DeleteFile "docs/content/license.md"
    CopyFile "docs/content/" "LICENSE.txt"
    Rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp' true true
)

Target "KeepRunning" (fun _ ->
    use watcher = !! "docs/content/**/*.*" |> WatchChanges (fun changes ->
         generateHelp' true true
    )

    traceImportant "Waiting for help edits. Press any key to stop."

    System.Console.ReadKey() |> ignore

    watcher.Dispose()
)

Target "GenerateDocs" DoNothing

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Git.Commit.Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "ReleaseGithub" (fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    //StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    let client =
        match Environment.GetEnvironmentVariable "OctokitToken" with
        | null -> 
            let user =
                match getBuildParam "github-user" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> getUserInput "Username: "
            let pw =
                match getBuildParam "github-pw" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> getUserPassword "Password: "

            createClient user pw
        | token -> createClientWithToken token

    // release on github
    client
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "BuildPackage" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Default" DoNothing
Target "Bundle"  DoNothing
Target "Release" DoNothing

"Clean"
  ==> "Restore"
  ==> "AssemblyInfo"
  ==> "Build.Emit"
  ==> "Build.NoEmit"
  ==> "Build"
  ==> "RunTests.Release"
  ==> "RunTests.Release-NoEmit"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "NuGet.Bundle"
  ==> "NuGet.ValidateSourceLink"
  ==> "Bundle"

"Bundle"
  ==> "NuGet.Push"
  ==> "ReleaseGithub"
  ==> "Release"

RunTargetOrDefault "Default"