namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("TypeShape")>]
[<assembly: AssemblyProductAttribute("TypeShape")>]
[<assembly: AssemblyDescriptionAttribute("Practical Generic Programming in F#")>]
[<assembly: AssemblyVersionAttribute("1.2")>]
[<assembly: AssemblyFileVersionAttribute("1.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.2"
    let [<Literal>] InformationalVersion = "1.2"
