open System
open System.IO
open Spectre.Console
open Spectre.Console.Cli

open git_guts

// --------------------------------------------------------------------

let _getPackFilename packFilename repoDir =
    if packFilename <> "" then
        packFilename
    else
        // auto-detect pack files
        let fnames = findRepoPacks repoDir
        match fnames.Length with
        | 0 -> failwith "No pack file was specified."
        | 1 -> fnames.[0]
        | _ -> failwith "Multiple pack files were found."

// --------------------------------------------------------------------

type AppSettings() =
    inherit CommandSettings()
    [<CommandOption( "-r|--repo <GIT-REPO>" )>]
    member val RepoDir = "." with get, set
    override this.Validate() =
        this.RepoDir <- Path.GetFullPath( this.RepoDir )
        if not ( Directory.Exists this.RepoDir ) then
            ValidationResult.Error( "Can't find git repo directory." )
        else
            ValidationResult.Success()

// FUDGE! We need this for the settings in AppSettings to be recognized :-/
type AppCommand() =
    inherit Command<AppSettings>()
    override this.Execute( ctx, settings ) =
        failwith "No command was specified."
        0

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type DumpRepoObjectSettings() =
    inherit AppSettings()
    [<CommandOption( "-o|--obj <OBJ-NAME>" )>]
    member val ObjName = "" with get, set

type DumpRepoObjectCommand() =
    inherit Command<DumpRepoObjectSettings>()
    override this.Execute( ctx, settings ) =
        if settings.ObjName = "" then
            failwith "Missing object name."
        dumpRepoObject settings.RepoDir settings.ObjName
        0

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type DumpPackFileSettings() =
    inherit AppSettings()
    [<CommandOption( "-f|--pack-file <PACK-FILE>" )>]
    member val PackFilename = "" with get, set

type DumpPackFileCommand() =
    inherit Command<DumpPackFileSettings>()
    override this.Execute( ctx, settings ) =
        let fname = _getPackFilename settings.PackFilename settings.RepoDir
        dumpPackFile fname
        0

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type DumpPackObjectSettings() =
    inherit AppSettings()
    [<CommandOption( "-f|--pack-file <PACK-FILE>" )>]
    member val PackFilename = "" with get, set
    [<CommandOption( "-o|--obj <OBJ-NAME>" )>]
    member val ObjName = "" with get, set

type DumpPackObjectCommand() =
    inherit Command<DumpPackObjectSettings>()
    override this.Execute( ctx, settings ) =
        if settings.ObjName = "" then
            failwith "Missing object name."
        let fname = _getPackFilename settings.PackFilename settings.RepoDir
        dumpPackObject fname settings.ObjName
        0

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type DumpStagingIndexSettings() =
    inherit AppSettings()
    [<CommandOption( "-f|--full" )>]
    member val FullDump = false with get, set

type DumpStagingIndexCommand() =
    inherit Command<DumpStagingIndexSettings>()
    override this.Execute( ctx, settings ) =
        dumpStagingIndex settings.RepoDir settings.FullDump
        0

// --------------------------------------------------------------------

[<EntryPoint>]
let main argv =

    // Good grief! >:-/ Spectre.Console only checks for a NO_COLOR env var to see if it should disable
    // colors (see ColorSystemDetector.Detect()), instead of checking if stdout is connected to a terminal,
    // so we do that here.
    if Console.IsOutputRedirected then
        disableSpectreCapabilities

    // parse the command-line arguments
    let app = CommandApp<AppCommand>()
    app.Configure( fun cfg ->
        cfg.SetApplicationName( System.AppDomain.CurrentDomain.FriendlyName ) |> ignore
        cfg.AddCommand<DumpRepoObjectCommand>( "dump-object" ).WithDescription(
            "Dump an object in a git repo."
        ) |> ignore
        cfg.AddCommand<DumpPackFileCommand>( "dump-packfile" ).WithDescription(
            "Dump a pack file."
        ) |> ignore
        cfg.AddCommand<DumpPackObjectCommand>( "dump-packobject" ).WithDescription(
            "Dump a pack object."
        ) |> ignore
        cfg.AddCommand<DumpStagingIndexCommand>( "dump-stagingindex" ).WithDescription(
            "Dump the staging index."
        ) |> ignore
    )
    app.Run( argv )
