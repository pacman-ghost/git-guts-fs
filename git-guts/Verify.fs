namespace git_guts

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

// --------------------------------------------------------------------

[<AutoOpen>]
module VerifyObjects =

    let private _adjustGitTreeOutput (objDump: string) =
        // adjust git's output for TREE objects so that it matches what we output
        let regex = Regex( @"^(\d+) [a-z]+ ([0-9a-f]{40})\s+(.+)$" )
        let buf = new StringBuilder()
        let reader = new StreamReader( new MemoryStream( Encoding.UTF8.GetBytes( objDump ) ) )
        while not reader.EndOfStream do
            let line = reader.ReadLine()
            let matches = regex.Matches( line )
            let groups = matches.[0].Groups
            let perms, objName = groups.[1].Value, groups.[2].Value
            let mutable path = groups.[3].Value
            // FUDGE! git outputs UTF-8 bytes as octal-encoded strings, not the bytes themselves?!
            if path.[0] = '"' && path.[path.Length-1] = '"' then
                let rec updatePath (partialPath: string) = seq {
                    let pos = partialPath.IndexOf( '\\' )
                    if pos < 0 then
                        yield! Encoding.UTF8.GetBytes( partialPath )
                    else
                        yield! Encoding.UTF8.GetBytes( partialPath.Substring( 0, pos ) )
                        let n = Convert.ToByte( partialPath.Substring( pos+1, 3 ), 8 )
                        yield byte( n )
                        yield! updatePath ( partialPath.Substring( pos+4 ) )
                }
                path <- Encoding.UTF8.GetString(
                    updatePath ( path.Substring( 1, path.Length-2 ) ) |> Seq.toArray
                )
            buf.AppendFormat( "{0} {1} {2}\n", perms, objName, path ) |> ignore
        buf.ToString().TrimEnd()

    let verifyObjects repoDir progress =

        // NOTE: This will iterate over every object in a repo, and compare what we retrieve with what
        // "git cat-file" returns. In particular, this will include *every* revision of *every* file,
        // so for large repo's, it will take some time...

        // initialize
        disableSpectreCapabilities ()
        let mutable currPackFname = ""
        let mutable packObjCounts = Map[ ( "", 0 ) ]

        let onEndPackFile () =
            let nObjs = packObjCounts.[ currPackFname ]
            printfn "- Checked %s." ( plural nObjs "object" "objects" )

        // check each object in the repo
        printfn "Checking loose objects..." // nb: because getObjNames returns loose objects first
        for objName, fname in getObjNames repoDir do

            // check if we have a loose object or an object in a pack
            let packFname =
                if Path.GetExtension( fname ) = ".pack" then Path.GetFileName( fname ) else ""

            // check if we've started a new pack
            if packFname <> currPackFname then
                // yup - log the end of the current one
                onEndPackFile ()
                // prepare to start processing the new pack file
                currPackFname <- packFname
                packObjCounts <- packObjCounts.Add ( currPackFname, 0 )
                let fsize = FileInfo( fname ).Length
                printfn ""
                printfn "Checking pack file (%s): %s" (friendlyByteCount fsize) currPackFname

            // find the next object
            if progress then
                eprintfn "- Checking object: %s" objName
            let objRec = _findRepoObjRec repoDir objName
            if objRec.IsNone then
                failwithf "Can't find object: %s" objName
            let mutable objData = objRec.Value.objData
            let obj = makeGitObject objRec.Value
            packObjCounts <- packObjCounts.Add ( currPackFname, packObjCounts.[currPackFname]+1 )
            if progress then
                eprintfn "  - Got %s: #bytes=%d" obj.objType objData.Length

            // check the object type
            let expectedObjType = ( runGitText repoDir "cat-file" [ "-t"; objName ] ).TrimEnd()
            if obj.objType <> expectedObjType then
                failwithf "Object type mismatch for %s: got \"%s\", expected \"%s\"." objName obj.objType expectedObjType

            // check the object data
            let mutable expectedObjData = ( runGit repoDir "cat-file" [ "-p"; objName ] )
            if obj.objType = "tree" then
                objData <- Encoding.UTF8.GetBytes(
                    using ( new CaptureStdout() ) ( fun cap ->
                        obj.dumpObj()
                        cap.getOutput.TrimEnd()
                    )
                )
                expectedObjData <- Encoding.UTF8.GetBytes(
                    _adjustGitTreeOutput ( Encoding.UTF8.GetString expectedObjData )
                )
            if objData <> expectedObjData then
                let dname = Path.GetTempPath()
                File.WriteAllBytes( Path.Join( dname, "git-content.expected" ), expectedObjData )
                File.WriteAllBytes( Path.Join( dname, "git-content.actual" ), objData )
                failwithf "Object data mismatch for %s." objName

        onEndPackFile ()

        // NOTE: These functions generate object names that are invalid (they contain a non-hex character),
        // but they will always compare greater/less than a valid name, based on the first byte, which will
        // help test how we use the fanout table, and the binary search through the table of object names.
        let makeObjName1 byte0 =
            sprintf "%02x%s!" byte0 (String('0',37))
        let makeObjName2 byte0 =
            sprintf "%02x%sz" byte0 (String('f',37))
        // NOTE: Also test with an object name that appears in the middle of the range, for a given first byte.
        let makeObjName3 byte0 =
            sprintf "%02x%s" byte0 "80808080808080808080808080808080808080"

        // verify looking up unknown objects
        printfn ""
        printfn "Checking unknown objects..."
        let mutable nObjs = 0
        [| makeObjName1; makeObjName2; makeObjName3 |] |> Seq.iter ( fun makeObjName ->
            for byte0 = 0 to 255 do
                let objName = makeObjName byte0
                let obj = findRepoObject repoDir objName
                if obj.IsSome then
                    failwithf "Unexpectedly found object: %s" objName
                nObjs <- nObjs + 1
        )
        printf "- Checked %s." ( plural nObjs "unknown object" "unknown objects." )

// --------------------------------------------------------------------

[<AutoOpen>]
module VerifyLogs =

    let verifyLogs repoDir =

        // verify reading each log file
        for ref, fname in _findLogFiles repoDir do
            printfn "Processing log file: %s" fname

            // run git to get the log entries for the current ref
            let ref2 =
                let ref2 = ref.Replace( Path.DirectorySeparatorChar, '/' )
                if ref2.Length >= 12 && ref2.Substring( 0, 11 ) = "refs/heads/" then
                    ref2.Substring( 11 )
                else
                    ref2
            let expected = ( runGitText repoDir "reflog" [| "show"; ref2 |] ).TrimEnd()

            // NOTE: git shows just enough of the object names for them to be unique, so we need
            // to figure out how much that is, so that we can generate the same output :-/
            // We assume the first line is a log entry, that starts with an abbreviated object name.
            let objNamePrefixLen = expected.IndexOf( ' ' )

            // extract the log entries for the current ref
            let buf = new StringBuilder()
            let mutable nLogEntries = 0
            _readLogFile fname |> Seq.rev |> Seq.iteri ( fun logEntryNo logEntry ->
                if logEntry.nextRef.IsSome then
                    let objNamePrefix = logEntry.nextRef.Value.Substring( 0, objNamePrefixLen )
                    buf.AppendFormat( "{0} {1}@{{{2}}}: {3}", objNamePrefix, ref2, logEntryNo, logEntry.entryType.Value ) |> ignore
                    if logEntry.msg.IsSome then
                        buf.AppendFormat( ": {0}", logEntry.msg.Value ) |> ignore
                    buf.AppendLine( "" ) |> ignore
                    nLogEntries <- nLogEntries + 1
            )
            let output = buf.ToString().TrimEnd()

            // compare what we extracted with the git output
            if output <> expected then
                let dname = Path.GetTempPath()
                File.WriteAllText( Path.Join( dname, "git-log.expected" ), expected )
                File.WriteAllText( Path.Join( dname, "git-log.actual" ), output )
                failwithf "Mismatched output for ref: %s" ref2
            printfn "- Checked %s." ( plural nLogEntries "log entry" "log entries" )
