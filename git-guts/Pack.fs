namespace git_guts

open System.IO
open System.Collections.Generic

// --------------------------------------------------------------------

[<AutoOpen>]
module Pack =

    let dumpPackFile fname =

        // initialize
        if not ( File.Exists fname ) then
            failwithf "Can't find pack file: %s" fname

        // figure out what to do
        let extn = Path.GetExtension( fname ).ToLower()
        if extn = ".pack" then
            _dumpPackDataFile fname
        else if extn = ".idx" then
            _dumpPackIndexFile fname
        else
            failwithf "Unknown pack file extension: %s" extn

    let readPackObject fname objName :GitObject option =
        let fpos = _findObjInPack fname objName
        if fpos.IsNone then
            None
        else
            // read the object from the pack
            let fname2 = changeExtn fname ".pack"
            use inp = new FileStream( fname2, FileMode.Open, FileAccess.Read, FileShare.Read )
            inp.Seek( int64( fpos.Value ), SeekOrigin.Begin ) |> ignore
            let objType, objData, fpos2 = _readPackObject inp
            Some ( makeGitObject objType objData )

    let dumpPackObject fname objName =

        // initialize
        if not ( File.Exists fname ) then
            failwithf "Can't find pack file: %s" fname

        // find the specified pack object
        let obj = readPackObject fname objName
        if obj.IsNone then
            failwith "Object not found."
        obj.Value.dumpObj()
