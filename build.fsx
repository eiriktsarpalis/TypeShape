// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#nowarn "85"
#r @"packages/build/FAKE/tools/FakeLib.dll"
#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"

open Fake
open Fake.Git
open Fake.ReleaseNotesHelper
open System

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Folder to deposit deploy artifacts
let artifactsDir = __SOURCE_DIRECTORY__ @@ "artifacts"

// Pattern specifying assemblies to be tested
let testProjects = "tests/*.Tests/*.??proj"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "eiriktsarpalis"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "TypeShape"

let testFramework = 
    match getBuildParam "testFramework" with
    | x when String.IsNullOrWhiteSpace x -> None
    | x -> Some x

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs [ "bin" ; artifactsDir ; "temp" ]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" DoNothing

let buildWithConfiguration config =
    DotNetCli.Build(fun c ->
        { c with
            Project = __SOURCE_DIRECTORY__
            Configuration = config
            AdditionalArgs = 
                [ 
                    "-p:GenerateAssemblyInfo=true"
                    "-p:Version=" + release.AssemblyVersion 
                ]
        })

Target "Build.Emit" (fun _ -> buildWithConfiguration "Release")
Target "Build.NoEmit" (fun _ -> buildWithConfiguration "Release-NoEmit")

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

let runTests config (proj : string) =
    DotNetCli.Test (fun c ->
        { c with
            Project = proj
            Configuration = config
            AdditionalArgs =
                [
                    yield "--no-build"
                    yield "--blame"
                    match testFramework with Some f -> yield "--framework" ; yield f | None -> ()
                    yield "-p:ParallelizeAssemblies=true"
                    yield "-p:ParallelizeTestCollections=true"
                    yield "--"
                    if EnvironmentHelper.isMono then yield "RunConfiguration.DisableAppDomain=true" // https://github.com/xunit/xunit/issues/1357
                ] })

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
            ReleaseNotes = toLines release.Notes })
)

Target "NuGet.ValidateSourceLink" (fun _ ->
    for nupkg in !! (artifactsDir @@ "*.nupkg") do
        DotNetCli.RunCommand
            (fun p -> { p with WorkingDir = __SOURCE_DIRECTORY__ @@ "tests" @@ "TypeShape.Tests" } )
            (sprintf "sourcelink test %s" nupkg)
)

Target "NuGet.Push" (fun _ ->
    Paket.Push(fun p ->
        { p with
            WorkingDir = artifactsDir })
)

// --------------------------------------------------------------------------------------
// Release Scripts

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