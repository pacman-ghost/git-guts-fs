namespace git_guts

open System
open System.IO
open System.Text

open Spectre.Console

// --------------------------------------------------------------------

// This records a single entry in a repo's staging index.
type StagingIndexEntry =
    {
        ctime: int * int // nb: seconds + nanoseconds
        mtime: int * int // nb: seconds + nanoseconds
        dev: int
        ino: int
        objType: int
        perms: uint16
        uid: int
        gid: int
        fileSize: int
        objName: string
        flags: uint16
        extendedFlags: uint16 option
        path: byte[] // nb: the encoding is unknown
    }

    static member private _OBJECT_TYPES = Map[ ( 0x08, "regular file" ); ( 0x0a, "symlink" ); ( 0x0e, "gitlink" ) ]
    static member private _FLAG_NAMES = Map[ (0x8000u, "assume-valid"); (0x4000u, "extended") ]
    static member private _EXTENDED_FLAG_NAMES = Map[ (0x4000u, "skip-worktree"); (0x2000u, "intent-to-add") ]

    member private this._flagsStr =
        // return the StagingIndexEntry's flags as a string
        let mutable vals = []
        let flags = uint( this.flags )
        let bflags = bitflagString flags 2 StagingIndexEntry._FLAG_NAMES
        if bflags.Length >= 10 then
            vals <- vals @ [ bflags.[ 0 .. bflags.Length-10 ] ]
        vals <- vals @ [ sprintf "stage=%d" ((flags &&& 0x3000u) >>> 12) ]
        // NOTE: The "name length" field is for the entry path name, not the object name.
        let namelen = flags &&& 0x0fffu
        vals <- vals @ [
            if namelen < 0xfffu then sprintf "namelen=%d" namelen else "namelen=0xFFF"
        ]
        let valsStr = String.Join( ", ", vals )
        let bflags2 = if bflags.Length >= 8 then bflags.Substring( bflags.Length-7, 6 ) else bflags
        sprintf "%s (%s)" valsStr bflags2

    member private this._xflagsStr =
        // return the StagingIndexEntry's extended flags as a string
        let xflags = uint( this.extendedFlags.Value )
        bitflagString xflags 2 StagingIndexEntry._EXTENDED_FLAG_NAMES

    member this.dumpObj fullDump =
        // NOTE: The encoding for the path is actually unknown :-/ Encoding.UTF8 uses replacement fallback.
        AnsiConsole.MarkupLine( "- path:  {0}", pathStr (Encoding.UTF8.GetString( this.path )) )
        AnsiConsole.MarkupLine( "- name:  {0}", objNameStr this.objName )
        printfn "- flags: %s" this._flagsStr
        if this.extendedFlags.IsSome then
            printfn "         %s" this._xflagsStr
        printfn "- type:  %s (%d)" StagingIndexEntry._OBJECT_TYPES.[this.objType] this.objType
        printfn "- size:  %d" this.fileSize
        printfn "- perms: %s" (permsString this.perms)
        if fullDump then
            printfn "- uid:   %d" this.uid
            printfn "- gid:   %d" this.gid
            let makeTimeStr timeVal =
                let epoch = DateTime( 1970, 1, 1, 0, 0, 0, DateTimeKind.Utc )
                let dt = epoch.AddSeconds( float( fst timeVal ) )
                sprintf "%s (%d.%09d)" (dt.ToLocalTime().ToString("yyyy-MM-dd HH:mm:ss")) (fst timeVal) (snd timeVal)
            printfn "- ctime: %s" (makeTimeStr this.ctime)
            printfn "- mtime: %s" (makeTimeStr this.mtime)
            printfn "- dev:   %d" this.dev
            printfn "- ino:   %d" this.ino

// --------------------------------------------------------------------

[<AbstractClass>]
type StagingIndexExtension () =
    // Base class for extensions stored in the staging index.

    abstract member extnSig: string
    abstract member dumpExtn: unit -> unit

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// Used to hold a TREE extension.
type TreeExtensionEntry =
    {
        path: byte[] // nb: the encoding is unknown
        nEntries: int
        nSubTrees: int
        objName: string option
    }

    member this.dumpEntry =
        // NOTE: The encoding for the path is actually unknown :-/ Encoding.UTF8 uses replacement fallback.
        AnsiConsole.MarkupLine( "- path = {0}", pathStr ( Encoding.UTF8.GetString this.path ) )
        printfn "  - entries:  %d" this.nEntries
        printfn "  - subtrees: %d" this.nSubTrees
        if this.objName.IsSome then
            AnsiConsole.MarkupLine( "  - name:     {0}", objNameStr this.objName.Value )

type TreeExtension( extnData: byte[] ) =
    inherit StagingIndexExtension ()

    override this.extnSig = "TREE"

    member val entries =
        // parse the TREE extension data
        use extnDataBuf = new MemoryStream( extnData )
        Seq.initInfinite ( fun _ ->
            if extnDataBuf.Position < extnDataBuf.Length then
                let path = readUntil extnDataBuf 0uy
                let nEntries = int( Encoding.ASCII.GetString( readUntil extnDataBuf 0x20uy ) )
                let nSubTrees = int( Encoding.ASCII.GetString( readUntil extnDataBuf 0x0auy ) )
                let objName = if nEntries <> -1 then Some( readObjName extnDataBuf ) else None
                Some { path=path; nEntries=nEntries; nSubTrees=nSubTrees; objName=objName }
            else
                None
        ) |> Seq.takeWhile ( fun e -> e.IsSome ) |> Seq.map ( fun e -> e.Value ) |> Seq.toArray

    override this.dumpExtn () =
        // dump the TREE extension
        for entry in this.entries do
            entry.dumpEntry

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// Used to hold a REUC extension.
type ReucExtensionEntry =
    {
        path: byte[] // nb: the encoding is unknown
        stages: ( string * string option )[] // perms + object name
    }

    member this.dumpEntry =
        // NOTE: The encoding for the path is actually unknown :-/ Encoding.UTF8 uses replacement fallback.
        AnsiConsole.MarkupLine( "- path = {0}", pathStr ( Encoding.UTF8.GetString this.path ) )
        for i = 0 to this.stages.Length-1 do
            let perms, objName = this.stages.[i]
            let objName2 = if objName.IsSome then sprintf " %s" objName.Value else ""
            AnsiConsole.MarkupLine( "  - stage {0}: {1}{2}", i+1, perms, objNameStr objName2 )

type ReucExtension( extnData: byte[] ) =
    inherit StagingIndexExtension ()

    override this.extnSig = "REUC"

    member val entries =
        // parse the REUC extension data
        use extnDataBuf = new MemoryStream( extnData )
        Seq.initInfinite ( fun _ ->
            if extnDataBuf.Position < extnDataBuf.Length then
                let path = readUntil extnDataBuf 0uy
                let getMode _ =
                    readString extnDataBuf "ascii" // nb: these are ASCII octal numbers
                let modes = Seq.initInfinite getMode |> Seq.take 3 |> Seq.toArray
                let getObjName mode =
                    if mode = "0" then None else Some( readObjName extnDataBuf )
                let objNames = modes |> Seq.map getObjName |> Seq.toArray
                let stages = Array.zip modes objNames
                Some { path=path; stages=stages }
            else
                None
        ) |> Seq.takeWhile ( fun e -> e.IsSome ) |> Seq.map ( fun e -> e.Value ) |> Seq.toArray

    override this.dumpExtn () =
        // dump the REUC extension
        for entry in this.entries do
            entry.dumpEntry

// --------------------------------------------------------------------

[<AutoOpen>]
module StagingIndex =

    let private _readStagingIndexHeader (inp: Stream) =

        // read the header
        let buf = readBytes inp 4
        if (Encoding.ASCII.GetString buf) <> "DIRC" then
            failwithf "Incorrect magic number: %A" buf
        let version = readNboInt4 inp
        if version <> 2 && version <> 3 && version <> 4 then
            failwithf "Unexpected version: %d" version

        version

    let private _readStagingIndexEntry (inp: Stream) version =

        // NOTE: Entries usually represent a file, but can sometimes refer to a directory (if sparse checkout
        // is enabled in cone mode, and the sparse index extension is enabled), in which case:
        // - mode = 040000
        // - has SKIP_WORKTREE in the extended flags
        // - the path ends with a directory separator
        // IMPORTANT! We assume we're not in split index mode (the entry format is completely different).

        let readPath () =
            if version = 4 then
                // NOTE: The path format is completely different in v4, so for simplicity, we don't support it.
                failwith "Version 4 is not supported."
            readUntil inp 0uy

        // read the next staging index entry
        let fposStart = inp.Position
        let ctime = ( readNboInt4 inp, readNboInt4 inp )
        let mtime = ( readNboInt4 inp, readNboInt4 inp )
        let dev = readNboInt4 inp
        let ino = readNboInt4 inp
        // NOTE: The doco says that the mode field is a 32-bit value, but only accounts for 16 of them :-/
        let mode = readBytes inp 4
        let objType = int( mode.[2] &&& 0xf0uy ) >>> 4
        let perms = uint16( mode.[2] &&& 0x01uy ) <<< 8 ||| uint16(mode.[3])
        let uid = readNboInt4 inp
        let gid = readNboInt4 inp
        let fileSize = readNboInt4 inp
        let objName = readObjName inp
        let flags = uint16( readNboInt2 inp )
        let extendedFlags =
            if version >= 3 && flags &&& 0x400us <> 0us then
                Some( uint16( readNboInt2 inp ) )
            else
                None
        let path = readPath ()

        // skip over the pad bytes (used to 8-align entries)
        if version <> 4 then
            while (inp.Position - fposStart) % 8L <> 0L do
                inp.ReadByte() |> ignore

        // create the StagingIndexEntry record
        let entry = {
            ctime=ctime; mtime=mtime
            dev=dev; ino=ino
            objType=objType
            perms=perms
            uid=uid; gid=gid
            fileSize=fileSize
            objName=objName
            flags=flags; extendedFlags=extendedFlags
            path=path
        }

        ( entry, fposStart )

    let private _makeStagingIndexExtension extnSig extnData =
        // create a StagingIndexExtension-derived object
        match extnSig with
            | [| 84uy; 82uy; 69uy; 69uy |] -> // "TREE"
                (TreeExtension extnData) :> StagingIndexExtension
            | [|82uy; 69uy; 85uy; 67uy|] -> // "REUC"
                (ReucExtension extnData) :> StagingIndexExtension
            | _ -> failwithf "Unknown extension sig: %A" extnSig

    let private _readExtension inp =
        // read the extension data
        let extnSig = readBytes inp 4
        let nBytes = readNboInt4 inp
        let extnData = readBytes inp nBytes
        let extn = _makeStagingIndexExtension extnSig extnData
        ( extn, extnData )

    let dumpStagingIndex repoDir fullDump =

        // initialize
        let fname = Path.Join( repoDir, ".git/index" )
        if not ( File.Exists( fname ) ) then
            failwith "Can't find the staging index file."
        use inp = new FileStream( fname, FileMode.Open, FileAccess.Read, FileShare.Read )

        // dump the header
        let version = _readStagingIndexHeader inp
        let nEntries = readNboInt4 inp
        AnsiConsole.MarkupLine( makeHeader "HEADER" "" )
        printfn ""
        printfn "version: %d" version
        printfn "entries: %d" nEntries

        // dump the entries
        printfn ""
        AnsiConsole.MarkupLine( makeHeader "ENTRIES" "" )
        for entryNo = 0 to nEntries-1 do
            printfn ""
            let entry, fpos = _readStagingIndexEntry inp version
            AnsiConsole.MarkupLine( sprintf "[cyan]Entry %d[/]: fpos=0x%x" entryNo fpos )
            entry.dumpObj fullDump

        // dump the extensions
        let fposEnd = inp.Length - 20L // we ignore the checksum at the end of the file
        Seq.initInfinite ( fun _ ->
            if inp.Position < fposEnd then
                let fposStart = inp.Position
                let extn, extnData = _readExtension inp
                Some ( extn, extnData, fposStart )
            else
                None
        ) |> Seq.takeWhile ( fun e -> e.IsSome ) |> Seq.map ( fun e -> e.Value ) |> Seq.iteri ( fun extnNo row ->
            let extn, extnData, fpos = row
            if extnNo = 0 then
                printfn ""
                AnsiConsole.MarkupLine( makeHeader "EXTENSIONS" "" )
            printfn ""
            AnsiConsole.MarkupLine( "[cyan]{0}[/]: fpos=0x{1:x}, #bytes={2}",
                extn.extnSig, fpos, extnData.Length
            )
            extn.dumpExtn ()
        )
