// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

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

// The name of the project on GitHub
let gitName = "TypeShape"

let testFramework = 
    match Environment.environVarOrDefault "testFramework" "" with
    | x when String.IsNullOrWhiteSpace x -> None
    | x -> Some x

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [ "bin" ; artifactsDir ; "temp" ]
)

Target.create "CleanDocs" (fun _ ->
    Shell.cleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" ignore

let buildWithConfiguration config =
    DotNet.build(fun c ->
        { c with
            Configuration = DotNet.BuildConfiguration.fromString config

            MSBuildParams =
                { c.MSBuildParams with
                    Properties = [("Version", release.NugetVersion)] }

        }) __SOURCE_DIRECTORY__

Target.create "Build.Emit" (fun _ -> buildWithConfiguration "Release")
Target.create "Build.NoEmit" (fun _ -> buildWithConfiguration "Release-NoEmit")

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

let runTests config =
    DotNet.test (fun c ->
        { c with
            Configuration = DotNet.BuildConfiguration.fromString config
            NoBuild = true
            Blame = true
            Framework = testFramework

            MSBuildParams =
                { c.MSBuildParams with
                    Properties = [("ParallelizeAssemblies", "true"); ("ParallelizeTestCollections", "true")] }

            RunSettingsArguments = 
                if Environment.isWindows then None
                else Some " -- RunConfiguration.DisableAppDomain=true" // https://github.com/xunit/xunit/issues/1357
        }) __SOURCE_DIRECTORY__

Target.create "RunTests" ignore
Target.create "RunTests.Release" (fun _ -> runTests "Release")
Target.create "RunTests.Release-NoEmit" (fun _ -> runTests "Release-NoEmit")

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet.Bundle" (fun _ ->
    let releaseNotes = String.toLines release.Notes |> System.Net.WebUtility.HtmlEncode
    DotNet.pack (fun pack ->
        { pack with
            OutputPath = Some artifactsDir 
            Configuration = DotNet.BuildConfiguration.Release
            MSBuildParams =
                { pack.MSBuildParams with
                    Properties = 
                        [("Version", release.NugetVersion)
                         ("PackageReleaseNotes", releaseNotes)] }
        }) __SOURCE_DIRECTORY__
)

Target.create "NuGet.ValidateSourceLink" (fun _ ->
    for nupkg in !! (artifactsDir @@ "*.nupkg") do
        let p = DotNet.exec id "sourcelink" (sprintf "test %s" nupkg)
        if not p.OK then failwithf "failed to validate sourcelink for %s" nupkg
)

Target.create "NuGet.Push" (fun _ ->
    DotNet.nugetPush (fun opts ->
        { opts with
            PushParams =
                { opts.PushParams with
                    NoSymbols = true
                    Source = Some "https://api.nuget.org/v3/index.json"
                    ApiKey = Some (Environment.GetEnvironmentVariable "NUGET_KEY") }
        }) (artifactsDir + "/*")
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
        match Environment.GetEnvironmentVariable "OctokitToken" with
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

Target.runOrDefault "Default"