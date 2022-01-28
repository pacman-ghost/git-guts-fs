namespace git_guts

open System.IO
open System.Text

open Spectre.Console

// --------------------------------------------------------------------

[<AutoOpen>]
module Pack =

    let private _readPackDataHeader (inp: Stream) =

        // read the header
        let buf = Array.zeroCreate 4
        inp.Read( buf, 0, 4 ) |> ignore
        if (Encoding.ASCII.GetString buf) <> "PACK" then
            failwithf "Incorrect magic number: %A" buf
        let version = readNboInt inp
        if version <> 2 then
            failwithf "Unsupported pack file version: %d" version
        let nObjs = readNboInt inp

        ( version, nObjs )

    let private _readPackObjects (inp: Stream) = seq {

        // read each object
        let endPos = inp.Length - 20L // nb: we ignore the 20-byte checksum at the end
        while inp.Position < endPos do

            // read the object type
            let fpos = inp.Position
            let byt = inp.ReadByte()
            let objType = (byt &&& 0x70) >>> 4

            // read the object size
            let rec getShiftedBytes byt bshift = seq {
                yield (byt &&& 0x7F) <<< bshift
                if byt &&& 0x80 <> 0 then
                    let byt2 = inp.ReadByte()
                    let bshift2 = if bshift = 0 then 4 else bshift+7
                    yield! getShiftedBytes byt2 bshift2
            }
            let foldShiftedByte acc shiftedByte =
                acc ||| shiftedByte
            let objSize = getShiftedBytes (byt &&& 0x8F) 0 |> Seq.fold foldShiftedByte 0

            // read the object data
            let mutable objData = null
            let obj =
                match objType with
                | 1 ->
                    objData <- decompressStream inp objSize
                    (CommitGitObject objData) :> GitObject
                | 2 ->
                    objData <- decompressStream inp objSize
                    (TreeGitObject objData) :> GitObject
                | 3 ->
                    objData <- decompressStream inp objSize
                    (BlobGitObject objData) :> GitObject
                | 4 ->
                    objData <- decompressStream inp objSize
                    (TagGitObject objData) :> GitObject
                | _ ->
                    failwithf "Unknown object type: %d" objType

            yield obj, fpos, objData
    }

    let dumpPackFile fname =

        // initialize
        if not ( File.Exists fname ) then
            failwithf "Can't find pack file: %s" fname
        use inp = new FileStream( fname, FileMode.Open, FileAccess.Read, FileShare.Read )

        // read the header
        let version, nObjs = _readPackDataHeader inp

        // dump each object
        _readPackObjects inp |> Seq.iteri ( fun objNo row ->
            let obj, fpos, objData = row
            AnsiConsole.MarkupLine( makeHeader
                ( sprintf "OBJECT %d: %s" objNo obj.objType )
                ( sprintf "(fpos=0x%x, size=%d)" fpos objData.Length )
            )
            printfn ""
            obj.dumpObj()
            printfn ""
        )
