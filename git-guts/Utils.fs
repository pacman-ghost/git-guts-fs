namespace git_guts

open System
open System.Text
open System.IO

open Spectre.Console

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

    let disableSpectreCapabilities =
        // disable colors (and other capabilities) in Spectre.Console
        AnsiConsole.Profile.Capabilities.ColorSystem <- ColorSystem.NoColors
        AnsiConsole.Profile.Capabilities.Ansi <- false
        AnsiConsole.Profile.Capabilities.Links <- false

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

    let pathStr path =
        // return the path display string
        match path with
            | "" -> "(empty)"
            | _ -> "[green]" + (if path.Substring(0,2) = "./" then path.Substring(2) else path) + "[/]"
