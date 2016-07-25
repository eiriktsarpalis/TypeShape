namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("TypeShape")>]
[<assembly: AssemblyProductAttribute("TypeShape")>]
[<assembly: AssemblyDescriptionAttribute("Experiments in Generic Programming")>]
[<assembly: AssemblyVersionAttribute("0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1"
    let [<Literal>] InformationalVersion = "0.1"
