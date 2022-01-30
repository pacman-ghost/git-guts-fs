namespace git_guts

open System
open System.IO
open System.Text.RegularExpressions
open Spectre.Console

// --------------------------------------------------------------------

type LogEntry =
    {
        entryType: string option
        prevRef: string option
        nextRef: string option
        userName: string
        userEmail: string
        tstamp: int * string // epoch timestamp + timezone
        msg: string option
    }

    member this.dumpLogEntry =
        AnsiConsole.Markup( "[cyan]{0}[/]",
            if this.entryType.IsSome then this.entryType.Value else "(no type)"
        )
        if this.msg.IsSome then
            printf ": %s" this.msg.Value
        printfn ""
        let dt = parseTimestamp ( fst this.tstamp )
        let tstamp = sprintf "%s %s" (dt.ToString "yyyy-MM-dd HH:mm:ss") (snd this.tstamp)
        printfn "%s (%s) %s" this.userName this.userEmail tstamp
        if this.prevRef.IsSome then
            AnsiConsole.Markup( "{0} ", objNameStr this.prevRef.Value )
        printf "->"
        if this.nextRef.IsSome then
            AnsiConsole.Markup( " {0}", objNameStr this.nextRef.Value )
        printfn ""

// --------------------------------------------------------------------

[<AutoOpen>]
module Logs =

    let private _findLogFiles repoDir = seq {
        // find log files in the specified repo
        let dname = Path.Join( repoDir, "/.git/logs" )
        if Directory.Exists( dname ) then
            let prefix = dname + Path.DirectorySeparatorChar.ToString()
            for fname in Directory.GetFiles( dname, "*", SearchOption.AllDirectories ) do
                if not ( fname.StartsWith( prefix ) ) then
                    failwithf "Unexpected log filename: %s" fname
                let ref = fname.Substring( prefix.Length )
                yield ref, fname
    }

    let private _readLogFile fname = seq {
        let regex = Regex( @"^([0-9a-f]{40}) ([0-9a-f]{40}) (.+?) \<(.+?)\> (\d+) ([+-]\d{4})(\s+[^:]+)?" )
        for line in File.ReadLines( fname ) do
            let line2 = line.Trim()
            let matches = regex.Matches( line2 )
            if matches.Count <> 1 then
                failwithf "Couldn't parse log line: %s" line2
            let groups = matches.[0].Groups
            let prevRef, nextRef = groups.[1].Value, groups.[2].Value
            let userName, userEmail = groups.[3].Value, groups.[4].Value
            let tstamp, tzone = groups.[5].Value, groups.[6].Value
            let entryType = if groups.[7].Success then Some( groups.[7].Value.Trim() ) else None
            let msg =
                if groups.[0].Length < line2.Length then
                    Some ( line2.Substring( groups.[0].Length + 2 ) )
                else
                    None
            let checkRef ref =
                if ref = "0000000000000000000000000000000000000000" then None else Some ref
            yield {
                entryType = entryType
                prevRef = checkRef prevRef; nextRef = checkRef nextRef
                userName = userName; userEmail = userEmail
                tstamp = ( int(tstamp), tzone )
                msg = msg
            }
    }

    let dumpLogs repoDir =

        // dump the log files (sorted, for stable output)
        _findLogFiles repoDir |> Seq.sortBy ( fun f -> fst f ) |> Seq.iteri ( fun logNo (ref, fname) ->
            if logNo > 0 then
                printfn ""
            AnsiConsole.MarkupLine( makeHeader ref "" )
            for entry in _readLogFile fname do
                printfn ""
                entry.dumpLogEntry
        )
