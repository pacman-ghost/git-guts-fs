namespace git_guts

open System
open System.Text
open System.IO
open System.Diagnostics

open Spectre.Console

// --------------------------------------------------------------------

type CaptureStdout () =
    // Temporarily capture output sent to stdout.

    // set up a buffer to capture stdout
    let _writer = new StringWriter()
    let _prevOut = System.Console.Out
    let _prevAnsiConsole = AnsiConsole.Console.Profile.Out
    do
        System.Console.SetOut( _writer )
        AnsiConsole.Console.Profile.Out <- new AnsiConsoleOutput( _writer )

    interface IDisposable with
        member this.Dispose() =
            // clean up
            System.Console.SetOut( _prevOut )
            AnsiConsole.Console.Profile.Out <- _prevAnsiConsole
            _writer.Dispose()

    member this.getOutput =
        // return the captured output
        _writer.ToString()

// --------------------------------------------------------------------

type StringBuilderBuf () =
    // Allow messages to be built up in a StringBuilder.

    // initialize
    let _buf = new StringBuilder()

    member this.printf fmt =
        // generate the message, then add it to the buffer
        let writeBuf (msg: string) =
            _buf.Append( msg ) |> ignore
        Printf.kprintf writeBuf fmt

    member this.endOfLine =
        _buf.AppendLine() |> ignore

    override this.ToString() =
        _buf.ToString()

// --------------------------------------------------------------------

[<AutoOpen>]
module Utils =

    let runGit repoDir cmd args =
        // run git and capture the output
        let gitPath = "git" // nb: we assume this is on the PATH
        let gitDir = Path.Combine( repoDir, ".git" )
        let startInfo = ProcessStartInfo( FileName=gitPath, RedirectStandardOutput=true, UseShellExecute=false )
        let addArg arg = startInfo.ArgumentList.Add( arg )
        Seq.iter addArg [| "--git-dir"; gitDir; cmd |]
        Seq.iter addArg args
        let proc = Process.Start( startInfo )
        let getBytes = Seq.initInfinite ( fun _ -> proc.StandardOutput.BaseStream.ReadByte() )
        let output = getBytes |> Seq.takeWhile ( fun b -> b <> -1 ) |> Seq.map ( fun b -> byte(b) ) |> Seq.toArray
        proc.WaitForExit()
        if proc.ExitCode <> 0 then
            failwithf "git failure: rc=%d" proc.ExitCode
        output

    let runGitText repoDir cmd args =
        // run git and capture the output as text
        Encoding.UTF8.GetString( runGit repoDir cmd args )

    let runGitGc repoDir =
        // run git garbage collection
        runGit repoDir "gc" [] |> ignore

    let disableSpectreCapabilities () =
        // disable colors (and other capabilities) in Spectre.Console
        AnsiConsole.Profile.Capabilities.ColorSystem <- ColorSystem.NoColors
        AnsiConsole.Profile.Capabilities.Ansi <- false
        AnsiConsole.Profile.Capabilities.Links <- false
        // FUDGE! Spectre.Console wraps output?!?!
        AnsiConsole.Profile.Width <- 99999

    let safeSpectreString (str: string) =
        // escape characters that have meaning for Spectre
        str.Replace( "[", "[[" ).Replace( "]", "]]" )

    let changeExtn (fname: string) newExtn =
        // change the filename's extension
        let dirName = Path.GetDirectoryName( fname )
        let fname2 = Path.GetFileNameWithoutExtension( fname ) + newExtn
        if dirName = String.Empty then
            fname2
        else
            Path.Join( dirName, fname2 )

    let makeHeader (caption: string) (caption2: string) =
        // generate a header string
        let buf = StringBuilderBuf()
        let nChars = 3 + 1 + caption.Length + ( if caption2 <> "" then 1+caption2.Length else 0 )
        buf.printf "--- [cyan]%s[/] " (safeSpectreString caption)
        if caption2 <> "" then
            buf.printf "%s " caption2
        buf.printf "%s" ( String( '-', Math.Max( 79-nChars, 3 ) ) )
        buf.ToString()

    let dumpBytes (data: byte[]) startIndex nBytes prefix =
        let buf = StringBuilderBuf()
        let endIndex = startIndex + nBytes - 1
        let startIndex0 = int( startIndex / 16 ) * 16
        for byteNoStart in [ startIndex0 .. 16 .. endIndex ] do
            buf.printf "%s%05x |" prefix byteNoStart
            for i = 0 to 15 do
                let byteNo = byteNoStart + i
                buf.printf " %s" (
                    if byteNo >= startIndex && byteNo <= endIndex then
                        sprintf "%02x" data.[byteNo]
                    else
                        ".."
                )
            buf.printf " | "
            for i = 0 to 15 do
                let byteNo = byteNoStart + i
                buf.printf "%c" (
                    if byteNo >= startIndex && byteNo <= endIndex then
                        if data.[byteNo] >= 32uy && data.[byteNo] < 127uy then
                            Convert.ToChar( data.[byteNo] )
                        else
                            '.'
                    else
                        ' '
                )
            buf.endOfLine
        buf.ToString()

    let blobStr blobData snip =
        // return the blob display string
        let enc = System.Text.Encoding.GetEncoding( "UTF-8", EncoderFallback.ExceptionFallback, DecoderFallback.ExceptionFallback )
        let textVal =
            try
                // try to convert the blob a string
                enc.GetString( blobData, 0, blobData.Length )
            with
                | :? DecoderFallbackException ->
                    // couldn't convert the blob to a string - dump it
                    dumpBytes blobData 0 blobData.Length ""
        // NOTE: If there is a lot of content, we show only the first and last few lines (to avoid
        // overwhelming the output), but this won't work too well if those lines are very long... :-/
        let lines = textVal.Split( "\n" )
        if lines.Length <= 10 || not snip then
            textVal
        else
            let getLines = seq {
                yield! Seq.take 5 lines
                yield String.Format( "  ...{0} lines snipped...", lines.Length-8 )
                yield! lines.[ lines.Length-5 .. lines.Length-1 ]
            }
            String.Join( "\n", getLines )

    let objNameStr objName =
        // return the object name display string
        "[yellow]" + objName + "[/]"

    let refStr ref =
        // return the ref display string
        "[green]" + ref + "[/]"

    let pathStr path =
        // return the path display string
        match path with
            | "" -> "(empty)"
            | _ -> "[green]" + (if path.Substring(0,2) = "./" then path.Substring(2) else path) + "[/]"

    let bitflagString (flags: uint) nBytes (flagNames: Map<uint,string>) =
        // convert the bitflags to a formatted string
        let fmt = sprintf "0x{0:x%d}" (2 * nBytes)
        let flagsStr = String.Format( fmt, flags )
        let checkBitflag (bflag, flagName) =
            flags &&& bflag <> 0u
        let fnames = Map.toSeq flagNames |> Seq.filter checkBitflag |> Seq.map snd |> Seq.toArray
        if fnames.Length = 0 then
            flagsStr
        else
            sprintf "%s (%s)" (String.Join( ", ", fnames )) flagsStr

    let permsString (perms: uint16) =
        // convert the file permission flags to a formatted string
        let permNames = "rwxrwxrwx"
        let permsStr = String.Join( "", seq {
            for flagNo = 0 to permNames.Length-1 do
                let bmask = uint16( 1 <<< (8 - flagNo) )
                yield if perms &&& bmask <> 0us then permNames.[flagNo] else '-'
        } )
        sprintf "%s (0x%x)" permsStr perms

    let readBytes (inp: Stream) nBytes =
        // read the specified number of bytes from the stream
        let buf = Array.zeroCreate nBytes
        let nBytesRead = inp.Read( buf, 0, nBytes )
        if nBytesRead <> nBytes then
            failwithf "Unexpected number of bytes read: %d/%d" nBytesRead nBytes
        buf

    let parseTimestamp tstamp =
        // parse a timestamp
        let epoch = DateTime( 1970, 1, 1, 0, 0, 0, DateTimeKind.Utc )
        epoch.AddSeconds( float( tstamp ) )

    let plural n val1 val2 =
        // return a pluralized string
        sprintf "%d %s" n ( if n = 1 then val1 else val2 )

    let friendlyByteCount nBytes =
        // return a friendly byte-count string
        if nBytes < 1024L then
            plural (int nBytes) "byte" "bytes"
        else if nBytes < 1024L * 1024L then
            sprintf "%.1f KB" ( float( nBytes ) / 1024.0 )
        else if nBytes < 1024L * 1024L * 1024L then
            sprintf "%.1f MB" ( float( nBytes ) / 1024.0 / 1024.0 )
        else
            sprintf "%.1f GB" ( float( nBytes ) / 1024.0 / 1024.0 / 1024.0 )
