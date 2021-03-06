namespace git_guts

open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

// --------------------------------------------------------------------

[<AutoOpen>]
module GitRepo =

    let rec _findRepoObjRec (repoDir: string) (objName: string) =

        // check if the object is loose
        let fname = Path.Join( repoDir, sprintf ".git/objects/%s/%s" objName.[0..1] objName.[2..] )
        if File.Exists( fname ) then

            // yup - get it directly from there
            use inp = new FileStream( fname, FileMode.Open, FileAccess.Read, FileShare.Read )
            let data = decompressEntireStream inp

            // extract the object type and size
            let pos = Seq.findIndex (fun byt -> byt = 0uy) data
            let header = Encoding.ASCII.GetString( data, 0, pos )
            let words = header.Split( ' ' )
            if words.Length <> 2 then
                failwithf "Unexpected loose object header: %s" header
            let objType = parseObjType words.[0]
            let objSize = int( words.[1] )

            // get the object data
            let objData = data.[ pos+1 .. ]
            if objData.Length <> objSize then
                failwithf "Unexpected object data size: %d/%d" objData.Length objSize

            // return the object
            Some { objType=objType; objData=objData }

        else

            // nope - check all the packs in the repo
            let fnames = findRepoPacks repoDir
            try
                fnames |> Seq.map (fun fname -> _readPackObjRec fname objName _findRepoObjRec) |> Seq.find (fun obj -> obj.IsSome)
            with
                | :? KeyNotFoundException -> None

    let findRepoObject repoDir objName =
        // find the specified object in the repo
        let objRec = _findRepoObjRec repoDir objName
        if objRec.IsNone then
            None
        else
            Some( makeGitObject objRec.Value )

    let dumpRepoObject repoDir objName =
        // find and dump the specified repo object
        let obj = findRepoObject repoDir objName
        if obj.IsNone then
            failwith "Object not found."
        obj.Value.dumpObj()

    let getObjNames repoDir = seq {

        // return all loose objects in the repo
        let dname = Path.Join( repoDir, ".git/objects" )
        let regex = Regex( @"/([0-9a-f]{2})/([0-9a-f]{38}$)" )
        for fname in Directory.GetFiles( dname, "*", SearchOption.AllDirectories ) do
            let fname2 = fname.Replace( Path.DirectorySeparatorChar, '/' )
            let matches = regex.Matches( fname2 )
            if matches.Count > 0 then
                let groups = matches.[0].Groups
                let objName = groups.[1].Value + groups.[2].Value
                yield objName, fname2

        // return all objects in each pack
        for fname in findRepoPacks repoDir do
            let mutable objs = [] // FUDGE! Can't yield from inside the onObject callback :-/
            _readPackIndexFile fname ( fun fanout -> () ) ( fun objNo nObjs objName crc offset ->
                objs <- objs @ [ ( objName, fname ) ]
            )
            yield! objs

    }

    let dumpPackFile fname =
        // FUDGE! This is a wrapper function that passes in the recursively-called _findRepoObjRec function :-/
        _dumpPackFile fname _findRepoObjRec

    let dumpPackObject fname objName =
        // FUDGE! This is a wrapper function that passes in the recursively-called _findRepoObjRec function :-/
        _dumpPackObject fname objName _findRepoObjRec
