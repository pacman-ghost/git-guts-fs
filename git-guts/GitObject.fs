namespace git_guts

open System.IO

open Spectre.Console

// --------------------------------------------------------------------

[<AbstractClass>]
type GitObject( objData: byte[] ) =
    // Base class for objects stored in a pack data file.

    abstract member objType: string
    abstract member dumpObj: unit -> unit

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type CommitGitObject( objData ) =
    inherit GitObject( objData )

    override this.objType = "commit"

    // NOTE: We should really parse the fields contained in the object data, but since
    // we're just dumping these things, we simply show it as a block of text.
    member private this._objData = objData

    override this.dumpObj () =
        // dump the COMMIT object
        printf "%s" (blobStr this._objData false)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type TreeGitObjectEntry = {
    perms: string
    path: string
    objName:string
}

type TreeGitObject( objData ) =
    inherit GitObject( objData )

    override this.objType = "tree"

    member val entries =
        // read the tree entries
        let objDataStream = new MemoryStream( objData )
        let rec readEntries () = seq {
            if objDataStream.Position < objDataStream.Length then
                let caption = readString objDataStream "utf-8"
                let pos = caption.IndexOf( " " )
                yield {
                    perms = caption.Substring( 0, pos ).PadLeft( 6, '0' )
                    path = caption.Substring( pos+1 )
                    objName = readObjName objDataStream
                }
                yield! readEntries ()
        }
        readEntries () |> Seq.toList

    override this.dumpObj () =
        // dump the TREE object
        for entry in this.entries do
            AnsiConsole.MarkupLine( "{0} {1} {2}",
                entry.perms,
                objNameStr entry.objName,
                pathStr entry.path
            )

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type BlobGitObject( objData ) =
    inherit GitObject( objData )

    override this.objType = "blob"
    member private this._objData = objData

    override this.dumpObj () =
        // dump the BLOB object
        printf "%s" (blobStr this._objData true)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type TagGitObject( objData ) =
    inherit GitObject( objData )

    override this.objType = "tag"
    member private this._objData = objData

    override this.dumpObj () =
        // dump the TAG object
        printf "%s" (blobStr this._objData false)

// --------------------------------------------------------------------

// This is used to hold the raw data for objects extracted from a pack.
type ObjRec = {
    objType: int
    objData: byte[]
}

[<AutoOpen>]
module GitObject =

    let makeGitObject objRec =
        // create a GitObject-derived object
        match objRec.objType with
        | 1 -> (CommitGitObject objRec.objData) :> GitObject
        | 2 -> (TreeGitObject objRec.objData) :> GitObject
        | 3 -> (BlobGitObject objRec.objData) :> GitObject
        | 4 -> (TagGitObject objRec.objData) :> GitObject
        | _ -> failwithf "Unknown object type: %d" objRec.objType

    let parseObjType objType =
        match objType with
        | "commit" -> 1
        | "tree" -> 2
        | "blob" -> 3
        | "tag" -> 4
        | _ -> failwithf "Unknown object type: %s" objType
