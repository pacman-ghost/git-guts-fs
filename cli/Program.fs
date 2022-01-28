open System
open System.IO
open Argu

open git_guts

// --------------------------------------------------------------------

type DumpPackFileArgs =
    | [<AltCommandLine("-f")>] Pack_Filename of packFilename:string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Pack_Filename _ -> "Pack file to dump."

type CliArguments =
    | [<AltCommandLine("-r")>] Repo of path:string
    | [<CliPrefix(CliPrefix.None)>] Dump_PackFile of ParseResults<DumpPackFileArgs>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Repo _ -> "specify the git repo directory."
            | Dump_PackFile _ -> "dump a pack file."

type CliExiter() =
    interface IExiter with
        member __.Name = "CliExiter"
        member __.Exit( msg, code ) =
            if code = ErrorCode.HelpText then
                // show the help text
                printfn "%s" msg
                exit 0
            else
                // show the error message (sans help text)
                let pos = msg.IndexOf "USAGE:"
                let msg2 = msg.Substring( 0, pos-1 )
                printfn "%s" msg2
                printfn "Use --help to get help."
                exit 1

// --------------------------------------------------------------------

[<EntryPoint>]
let main argv =

    // Good grief! >:-/ Spectre.Console only checks for a NO_COLOR env var to see if it should disable
    // colors (see ColorSystemDetector.Detect()), instead of checking if stdout is connected to a terminal,
    // so we do that here.
    if Console.IsOutputRedirected then
        disableSpectreCapabilities

    // parse the command-line arguments
    let programName = System.AppDomain.CurrentDomain.FriendlyName
    let parser = ArgumentParser.Create<CliArguments>( programName=programName, helpTextMessage="Examine the guts of a git repo.", errorHandler=CliExiter() )
    let parsedArgs = parser.Parse argv
    let repoDir = Path.GetFullPath( parsedArgs.GetResult( Repo, defaultValue="." ) )
    if not ( Directory.Exists repoDir ) then
        failwith "Can't find git repo directory."

    // perform the requested action
    if parsedArgs.Contains Dump_PackFile then
        // dump a pack file
        let args = parsedArgs.GetResult Dump_PackFile
        if not ( args.Contains Pack_Filename ) then
            failwith "No pack file was specified." // nb: because [<Mandatory>] doesn't seem to work :-/
        let fname = args.GetResult Pack_Filename
        dumpPackFile fname
    else
        // no action was specified - print help
        printfn "%s" ( parser.PrintUsage() )

    0
