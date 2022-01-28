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

        // NOTE: .NET 6 has ZLibStream, but we're using .NET 5, so we use DeflateStream (but we have to skip
        // the 2-byte zlib header).
        let skip1 = inp.ReadByte()
        let skip2 = inp.ReadByte()
        if skip1 <> 0x78 || ( skip2 <> 0x9c && skip2 <> 0x01 ) then // nb: this declares zlib's compression setting
            failwithf "Unexpected zlib header: %x %x" skip1 skip2

        // decompress the specified number of bytes
        let buf = Array.zeroCreate nBytes
        if nBytes = 0 then
            // FUDGE! In Python, we ask zlib to decompress bytes from the stream until it's done, but in .NET,
            // we have to tell it how many bytes of uncompressed data we want, which doesn't seem to work when
            // the byte count is 0 :-/ I'm not sure if skipping bytes like this is right, but it'll do for now...
            for i = 0 to 5 do
                inp.ReadByte() |> ignore
            [||]
        else
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
            if byt = -1 then
                failwithf "Unexpected EOF."
            if byte( byt ) <> endByte then
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

    let readVliBe (inp: Stream) isOffsetEncoding =
        // read a variable-length integer (big-endian)
        let rec getBytes() = seq {
            let byt = inp.ReadByte()
            if byt = -1 then
                failwithf "Unexpected EOF."
            let isLastByte = ( byt &&& 0x80 = 0 )
            yield byte( byt &&& 0x7f ), isLastByte
            if not isLastByte then
                yield! getBytes()
        }
        let foldByte acc (byt, isLastByte) =
            let acc2 = ( acc <<< 7 ) ||| int( byt )
            if isOffsetEncoding && not isLastByte then
                // NOTE: When reading offsets for delta'fied objects, there is an additional twist :-/
                // The sequences [ 0xxxxxxx ] and [ 10000000, 0xxxxxxx ] would normally be read as
                // the same value (0xxxxxxx), so for each byte except the last one, we add 2^7,
                // which has the effect of ensuring that all 1-byte sequences are less than all 2-byte
                // sequences, which are less than all 3-byte sequences, etc. We add 1 here, but since
                // we are going to loop back and left-shift acc by 7 bits, that is the same as adding 2^7.
                // Look for "offset encoding" here:
                //   https://git-scm.com/docs/pack-format
                acc2 + 1
            else
                acc2
        getBytes() |> Seq.fold foldByte 0

    let readVliLe (inp: Stream) =
        // read a variable-length integer (little-endian)
        let rec getShiftedBytes bshift = seq {
            let byt = inp.ReadByte()
            if byt = -1 then
                failwith "Unexpected EOF."
            yield (byt &&& 0x7f) <<< bshift
            if byt &&& 0x80 <> 0 then
                yield! getShiftedBytes (bshift+7)
        }
        let foldShiftedByte acc shiftedByte =
            acc ||| shiftedByte
        getShiftedBytes 0 |> Seq.fold foldShiftedByte 0

    let findRepoPacks repoDir =
        // find pack files in the git repo
        let dname = Path.Combine( repoDir, ".git/objects/pack" )
        Directory.GetFiles( dname, "*.pack" )
