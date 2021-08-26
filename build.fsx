// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "nuget: System.Reactive        ,5.0.0"
#r "nuget: Fake.Core.UserInput    ,5.20.4"
#r "nuget: Fake.Core.ReleaseNotes ,5.20.4"
#r "nuget: Fake.Core.Target       ,5.20.4"
#r "nuget: Fake.IO.FileSystem     ,5.20.4"
#r "nuget: Fake.DotNet.Cli        ,5.20.4"
#r "nuget: Fake.Tools.Git         ,5.20.4"
#r "nuget: Fake.Api.Github        ,5.20.4"

#if !FAKE
Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__ []
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext
#endif

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools
open Fake.Api
open System

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Folder to deposit deploy artifacts
let artifactsDir = __SOURCE_DIRECTORY__ @@ "artifacts"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "eiriktsarpalis"
let gitHome = "https://github.com/" + gitOwner
let gitName = "TypeShape"

let configuration = Environment.environVarOrDefault "configuration" "Release"
let noEmitConfiguration = "Release-NoEmit"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [ "bin" ; artifactsDir ; "temp" ; "docs/output" ]
)

// --------------------------------------------------------------------------------------
// Build library & test project

let Build configuration =
    DotNet.build(fun c ->
        { c with
            Configuration = DotNet.BuildConfiguration.fromString configuration

            MSBuildParams =
                { c.MSBuildParams with
                    Properties = [("Version", release.NugetVersion)]
                    DisableInternalBinLog = true }

        }) __SOURCE_DIRECTORY__


Target.create "Build" (fun _ -> Build configuration)
Target.create "Build.NoEmit" (fun _ -> Build noEmitConfiguration)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

let Test configuration =
    DotNet.test (fun c ->
        { c with
            Configuration = DotNet.BuildConfiguration.fromString configuration
            NoBuild = true
            Blame = true

            MSBuildParams =
                { c.MSBuildParams with
                    Properties = [("ParallelizeAssemblies", "true"); ("ParallelizeTestCollections", "true")]
                    DisableInternalBinLog = true }

        }) __SOURCE_DIRECTORY__

Target.create "RunTests" (fun _ -> Test configuration)
Target.create "RunTests.NoEmit" (fun _ -> Test noEmitConfiguration)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet.Bundle" (fun _ ->
    let releaseNotes = String.toLines release.Notes |> System.Net.WebUtility.HtmlEncode
    DotNet.pack (fun pack ->
        { pack with
            OutputPath = Some artifactsDir 
            Configuration = DotNet.BuildConfiguration.fromString configuration
            MSBuildParams =
                { pack.MSBuildParams with
                    Properties = 
                        [("Version", release.NugetVersion)
                         ("PackageReleaseNotes", releaseNotes)]
                    DisableInternalBinLog = true }
        }) __SOURCE_DIRECTORY__
)

Target.create "NuGet.ValidateSourceLink" (fun _ ->
    for nupkg in !! (artifactsDir @@ "*.nupkg") do
        let p = DotNet.exec id "sourcelink" (sprintf "test %s" nupkg)
        if not p.OK then failwithf "failed to validate sourcelink for %s" nupkg
)

Target.create "NuGet.Push" (fun _ ->
    let source = "https://api.nuget.org/v3/index.json"
    let key = Environment.GetEnvironmentVariable "NUGET_KEY"
    for artifact in !! (artifactsDir + "/*nupkg") do
        let result = DotNet.exec id "nuget" (sprintf "push -s %s -k %s %s" source key artifact)
        if not result.OK then failwith "failed to push packages"
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target.create "ReleaseGithub" (fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    //StageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    let client =
        match Environment.GetEnvironmentVariable "GITHUB_TOKEN" with
        | null -> 
            let user =
                match Environment.environVarOrDefault "github-user" "" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> UserInput.getUserInput "Username: "
            let pw =
                match Environment.environVarOrDefault "github-pw" "" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> UserInput.getUserPassword "Password: "

            GitHub.createClient user pw
        | token -> GitHub.createClientWithToken token

    client
    |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> GitHub.publishDraft
    |> Async.RunSynchronously
)

Target.create "BuildPackage" ignore

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "Default" ignore
Target.create "Bundle"  ignore
Target.create "Release" ignore

"Clean"
  ==> "Build"
  ==> "RunTests"
  ==> "Build.NoEmit"
  ==> "RunTests.NoEmit"
  ==> "Default"

"Default"
  ==> "NuGet.Bundle"
  //==> "NuGet.ValidateSourceLink"
  ==> "Bundle"

"Bundle"
  ==> "NuGet.Push"
  ==> "ReleaseGithub"
  ==> "Release"

Target.runOrDefault "Default"