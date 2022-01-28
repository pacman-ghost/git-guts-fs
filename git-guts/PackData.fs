namespace git_guts

open System
open System.IO
open System.Text

open Spectre.Console

// --------------------------------------------------------------------

[<AutoOpen>]
module PackData =

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

    let private _makeDeltaObj (inp: Stream) (baseObjData: byte[]) objSize =

        // read the transformation data
        let tdata = decompressStream inp objSize
        if tdata.Length <> objSize then
            failwithf "Transformation data size mismatch: %d/%d" tdata.Length objSize

        // set up a new input stream for the transformation data
        let tdataStream = new MemoryStream( tdata )
        let baseObjSize = readVliLe tdataStream
        let objSize2 = readVliLe tdataStream

        // process the delta commands
        let rec getBytes() = seq {
            let byt = tdataStream.ReadByte()
            if byt = 0 then
                yield! getBytes()
            else if byt <> -1 then
                if byt &&& 0x80 <> 0 then
                    // copy data from the base object
                    let vals = Array.zeroCreate 7
                    for i = 0 to 6 do
                        if byt &&& (1 <<< i) <> 0 then
                            vals.[i] <- tdataStream.ReadByte()
                    let start = vals.[0] ||| (vals.[1] <<< 8) ||| (vals.[2] <<< 16) ||| (vals.[3] <<< 24)
                    let mutable nBytes = vals.[4] ||| (vals.[5] <<< 8) ||| (vals.[6] <<< 16)
                    if nBytes = 0 then
                        nBytes <- 0x10000
                    yield! baseObjData.[ start .. start+nBytes-1 ]
                    yield! getBytes()
                else
                    // add new data
                    let nBytes = byt &&& 0x7f
                    let buf = Array.zeroCreate nBytes
                    tdataStream.Read( buf, 0, nBytes ) |> ignore
                    yield! buf
                    yield! getBytes()
        }
        let objData = getBytes() |> Seq.toArray
        if objData.Length <> objSize2 then
            failwithf "Delta'fied object size mismatch: %d/%d" objData.Length objSize2

        objData

    let rec internal _readPackObject (inp: Stream) =

        // remember where the object starts in the pack data
        let fpos = inp.Position

        // read the object type
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
        let mutable objType2 = -1
        let mutable objData = null
        let obj =
            match objType with
            | 1 | 2 | 3 | 4 -> // commit, tree, blob, tag
                objData <- decompressStream inp objSize
                objType2 <- objType
            | 6 -> // ofs_delta
                let deltaObjAndType = _readOfsDeltaObj inp fpos objSize
                objData <- snd deltaObjAndType
                objType2 <- fst deltaObjAndType
            | _ ->
                failwithf "Unknown object type: %d" objType

        ( objType2, objData, fpos )

    and private _readOfsDeltaObj (inp: Stream) fpos objSize =

        // read the base object offset
        let offset = int64( readVliBe inp true )
        let baseObjOffset = fpos - offset

        // get the base object
        // IMPORTANT: The base object could itself be delta'fied.
        let prevPos = inp.Position
        inp.Seek( baseObjOffset, SeekOrigin.Begin ) |> ignore
        let baseObjType, baseObjData, fpos = _readPackObject inp
        inp.Seek( prevPos, SeekOrigin.Begin ) |> ignore

        // reconstruct the delta'fied object
        let objData = _makeDeltaObj inp baseObjData objSize

        ( baseObjType, objData )

    let private _readPackObjects (inp: Stream) = seq {
        // read each object
        let endPos = inp.Length - 20L // nb: we ignore the 20-byte checksum at the end
        while inp.Position < endPos do
            let objType, objData, fpos = _readPackObject inp
            let obj = makeGitObject objType objData
            yield obj, fpos, objData
    }

    let internal _dumpPackDataFile fname =

        // initialize
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
