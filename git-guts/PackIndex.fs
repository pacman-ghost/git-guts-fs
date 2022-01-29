namespace git_guts

open System
open System.IO

open Spectre.Console

// --------------------------------------------------------------------

[<AutoOpen>]
module PackIndex =

    let private _readPackIndexHeader (inp: Stream) =

        // read the header
        // NOTE: v1 index files don't have a version number, and just start with the fanout table, but
        // the magic number we check for here is an unreasonable value for fanout[0], so this is how
        // we detect the different versions.
        let buf = readBytes inp 4
        if buf <> [|0xffuy; 116uy; 79uy; 99uy|] then // nb: 0xff, then "tOc"
            failwithf "Incorrect magic number: %A" buf
        let version = readNboInt4 inp
        if version <> 2 then
            //  NOTE: Version 1 is also valid, but quite old, and we don't support it.
            failwithf "Unsupported pack index version: %d" version

        version

    let internal _findObjInPack fname (objName: string) =

        // initialize
        let fname2 = changeExtn fname ".idx"
        use inp = new FileStream( fname2, FileMode.Open, FileAccess.Read, FileShare.Read )

        // read the header
        let version = _readPackIndexHeader inp

        // read the fanout table
        let byte0 = Int32.Parse( objName.[0..1], Globalization.NumberStyles.AllowHexSpecifier )
        inp.Seek( int64( 4*byte0 ), SeekOrigin.Current ) |> ignore
        let endIndex = readNboInt4 inp
        let startIndex =
            if byte0 = 0 then
                0
            else
                inp.Seek( -8L, SeekOrigin.Current ) |> ignore
                readNboInt4 inp
        // NOTE: We now have two indexes into the table of object names:
        // - startIndex = index of an object name <= the name we're looking for
        // - endIndex = index of an object name > the name we're looking for

        // do a binary search for the target object name
        let fposNames = 4 + 4 + 4*256 // nb: offset to the start of the object names
        let seekTo fpos =
            inp.Seek( int64(fpos), SeekOrigin.Begin ) |> ignore
        let rec doSlice startIndex endIndex =
            // check if we're done
            if startIndex > endIndex then
                None // yup
            else
                // nope - slice the search range in two, and read the object name at the mid-point
                let midIndex = ( startIndex + endIndex ) / 2
                seekTo ( fposNames + 20 * midIndex )
                let midObjName = readObjName inp
                // check if we found the target object name
                if midObjName = objName then
                    // yup - get the object's offset in the pack data
                    seekTo ( fposNames - 4 )
                    let nObjs = readNboInt4 inp // nb: this is the last value in the fanout table
                    seekTo ( fposNames + 20*nObjs + 4*nObjs + 4*midIndex )
                    Some ( readNboInt4 inp )
                else
                    // nope - continue the binary search
                    if midObjName < objName then
                        doSlice (midIndex+1) endIndex
                    else
                        doSlice startIndex (midIndex-1)
        doSlice startIndex endIndex

    let readPackIndexObject (inp: Stream) objNo nObjs =

        // initialize
        let fposNames = 4 + 4 + 4*256
        let fposCrcs = fposNames + 20 * nObjs
        let fposOffsets = fposCrcs + 4 * nObjs
        let seekTo baseOffset recSize objNo =
            let fpos = int64( baseOffset + recSize * objNo )
            inp.Seek( fpos, SeekOrigin.Begin ) |> ignore

        // read the specified object
        seekTo fposNames 20 objNo
        let objName = readObjName inp
        seekTo fposCrcs 4 objNo
        let crc = readNboInt4 inp
        seekTo fposOffsets 4 objNo
        let offset = readNboInt4 inp
        if offset &&& 0x80000000 <> 0 then
            failwithf "Large offsets are not supported."

        ( objName, crc, offset )

    let internal _dumpPackIndexFile fname =

        // initialize
        use inp = new FileStream( fname, FileMode.Open, FileAccess.Read, FileShare.Read )

        // read the header
        let version = _readPackIndexHeader inp

        // read the fanout table
        let getFanoutVals = Seq.initInfinite (fun n -> readNboInt4 inp)
        let fanout = Seq.take 256 getFanoutVals |> Seq.toArray
        let nObjs = fanout.[255]

        // dump the fanout table header
        AnsiConsole.MarkupLine( "{0}", makeHeader "FANOUT" "" )
        printfn ""
        let fieldWidth = Math.Max( String.Format( "{0}", nObjs ).Length, 2 )
        let fmt = String.Format( "{{0,{0}}}", fieldWidth )
        printf "   "
        for col = 0 to 15 do
            let iVal = String.Format( "{0:x2}", col )
            printf " %s" ( String.Format( fmt, iVal ) )
        printfn ""
        printf "   "
        for col = 0 to 15 do
            let ruler = String( '-', fieldWidth )
            printf " %s" ruler
        printfn ""

        // dump the fanout table
        for row = 0 to 15 do
            printf "%02x:" (16 * row)
            for col = 0 to 15 do
                let fanoutVal = String.Format( fmt, fanout.[16*row+col] )
                printf " %s" fanoutVal
            printfn ""

        // dump the objects
        let fieldWidth2 = String.Format( "{0}", nObjs ).Length
        printfn ""
        let hdr = sprintf "OBJECTS (%d)" nObjs
        AnsiConsole.MarkupLine( "{0}", makeHeader hdr "" )
        printfn ""
        let prefix = String( ' ', fieldWidth2 )
        printfn "%s  name                                     crc      offset" prefix
        printfn "%s  ---------------------------------------- -------- --------" prefix
        let fmt = sprintf "{0,%d}: {1} {2:x8} 0x{3:x}" fieldWidth2
        for objNo = 0 to nObjs-1 do
            let objName, crc, offset = readPackIndexObject inp objNo nObjs
            AnsiConsole.MarkupLine( fmt,
                objNo, objNameStr objName, crc, offset
            )
