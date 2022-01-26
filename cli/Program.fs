open System
open System.IO
open Argu
open git_guts

type CliArguments =
    | [<AltCommandLine("-r")>] Repo of path:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Repo _ -> "specify the git repo directory."

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

[<EntryPoint>]
let main argv =

    // parse the command-line arguments
    let programName = System.AppDomain.CurrentDomain.FriendlyName
    let parser = ArgumentParser.Create<CliArguments>( programName=programName, helpTextMessage="Examine the guts of a git repo.", errorHandler=CliExiter() )
    let results = parser.Parse argv
    let repoDir = Path.GetFullPath( results.GetResult( Repo, defaultValue="." ) )
    if not (Directory.Exists repoDir) then
        failwith "Can't find git repo directory."

    // initialize
    let gitGuts = GitGuts( repoDir )
    printfn "git guts: %A" gitGuts.repoDir

    0
