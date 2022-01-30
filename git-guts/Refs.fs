namespace git_guts

open System.IO
open System.Text

open Spectre.Console

// --------------------------------------------------------------------

[<AutoOpen>]
module Refs =

    let private _findLooseRefs repoDir refType = seq {
        // find loose refs in the git repo
        let refsDir = Path.Join( repoDir, ".git/refs", refType )
        if Directory.Exists refsDir then
            for fname in Directory.GetFiles( refsDir ) do
                let objName = File.ReadAllText( fname, Encoding.ASCII ).Trim()
                yield ( Path.GetFileName fname, objName )
    }

    let private _findPackedRefs repoDir = seq {
        // find packed refs in the git repo
        let fname = Path.Join( repoDir, ".git/packed-refs" )
        if File.Exists fname then
            let mutable currRef = None
            for line in File.ReadLines( fname ) do
                let line2 = line.Trim()
                if line2.Length > 0 && line2.[0] <> '#' then
                    if line2.[0] = '^' then
                        // the previous line is an annotated tag, this line is the target commit
                        yield ( fst currRef.Value, snd currRef.Value, Some (line2.Substring 1) )
                        currRef <- None
                    else
                        // the previous line was a normal tag - we can now return it to the caller
                        if currRef.IsSome then
                            yield ( fst currRef.Value, snd currRef.Value, None )
                        // save the current line (to be yield'ed later)
                        // NOTE: We can't yield the tag now because it might be an annotated tag,
                        // in which case we need to wait for the next line to get the target commit.
                        currRef <- Some ( line2.Substring(41), line2.Substring(0,40) )
            if currRef.IsSome then
                yield ( fst currRef.Value, snd currRef.Value, None )
    }

    let dumpRefs repoDir =

        // dump the loose refs
        for refType in [| "heads"; "tags" |] do
            AnsiConsole.MarkupLine( makeHeader refType "" )
            let looseRefs = _findLooseRefs repoDir refType |> Seq.sortBy ( fun r -> fst r ) |> Seq.toList
            if looseRefs.Length > 0 then
                printfn ""
            for ref, objName in looseRefs do
                AnsiConsole.MarkupLine( "{0} -> {1}", refStr ref, objNameStr objName )
            printfn ""

        // dump the packed refs
        AnsiConsole.MarkupLine( makeHeader "packed refs" "" )
        printfn ""
        let packedRefs = _findPackedRefs repoDir |> Seq.sortBy ( fun (r, _, _) -> r )
        for ref, objName, target in packedRefs do
            AnsiConsole.Markup( "{0} -> {1}", refStr ref, objNameStr objName )
            if target.IsSome then
                AnsiConsole.Markup( " -> {0}", objNameStr target.Value )
            printfn ""
