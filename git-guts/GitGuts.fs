namespace git_guts

open System
open System.IO
open System.IO.Compression
open System.Reflection
open System.Text

// --------------------------------------------------------------------

[<AutoOpen>]
module GitGuts =

    let decompressStream (inp: Stream) nBytes =

        // decompress the specified number of bytes
        let buf = Array.zeroCreate nBytes
        // NOTE: .NET 6 has ZLibStream, but we're using .NET 5, so we use DeflateStream (but we have to skip
        // the 2-byte zlib header).
        inp.ReadByte() |> ignore
        inp.ReadByte() |> ignore
        use zstream = new DeflateStream( inp, CompressionMode.Decompress, true )
        let nBytesRead = zstream.Read( buf, 0, nBytes )
        if nBytesRead <> nBytes then
            failwithf "Unexpected number of decompressed bytes: %d/%d" nBytesRead nBytes

        // NOTE: We want the file pointer for the underlying Stream to be left pointing to the first byte
        // after the compressed data, but unfortunately, DeflateStream reads from the input stream in blocks:
        //   https://source.dot.net/#System.IO.Compression/System/IO/Compression/DeflateZLib/DeflateStream.cs
        // Mark Adler was less than impressed (as am I :-/):
        //   https://stackoverflow.com/questions/46238944/read-a-deflate-stream-until-adler32-checksum#comment79475895_46238944
        //   If what you're saying is true, that it doesn't stop at the end of the deflate stream, then the .NET DeflateStream
        //   decompression implementation is brain dead and useless. Based on Microsoft's past history with these classes,
        //   I would completely believe that.
        // We hack around this by checking how many bytes are still in the input buffer, and adjusting the file pointer
        // backwards by that amount. Sigh...
        let inflater = typeof<DeflateStream>.GetField( "_inflater", BindingFlags.NonPublic ||| BindingFlags.Instance ).GetValue( zstream )
        let zlibStream = inflater.GetType().GetField( "_zlibStream", BindingFlags.NonPublic ||| BindingFlags.Instance ).GetValue( inflater )
        let availInMethod = zlibStream.GetType().GetProperty( "AvailIn" ).GetMethod
        let availIn: uint32 = unbox( availInMethod.Invoke( zlibStream, null ) )
        // NOTE: Not sure why we need to adjust by 4 bytes, but it seems to work :-/
        inp.Seek( -(int64 availIn) + 4L, SeekOrigin.Current ) |> ignore

        buf

    let readUntil (inp: Stream) (endByte: byte) =
        // read bytes until the specified end byte is seen
        let rec readBytes () = seq {
            let byt = inp.ReadByte()
            if byt <> -1 && byte(byt) <> endByte then
                yield byte( byt )
                yield! readBytes ()
        }
        readBytes () |> Seq.toArray

    let readString inp (encoding: string) =
        // read a NULL-terminated string
        let str = readUntil inp 0uy
        Encoding.GetEncoding( encoding ).GetString( str, 0, str.Length )

    let readObjName (inp: Stream) =
        // read an object name (20 raw bytes) and return it as a hex-string
        let buf = Array.zeroCreate 20
        inp.Read( buf, 0, 20 ) |> ignore
        Convert.ToHexString( buf ).ToLower()

    let readNboInt (inp: Stream) =
        // read a network-byte-order int
        let buf = Array.zeroCreate 4
        inp.Read( buf, 0, 4 ) |> ignore
        ( int(buf.[0]) <<< 24 ) ||| ( int(buf.[1]) <<< 16 ) ||| ( int(buf.[2]) <<< 8 ) ||| int(buf.[3])

    let findRepoPacks repoDir =
        // find pack files in the git repo
        let dname = Path.Combine( repoDir, ".git/objects/pack" )
        Directory.GetFiles( dname, "*.pack" )
