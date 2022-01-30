namespace tests

open System
open System.Text
open System.IO
open System.IO.Compression
open Microsoft.VisualStudio.TestTools.UnitTesting

open Spectre.Console

open git_guts

// --------------------------------------------------------------------

type TempDir () =
    // Create a temp directory, and clean it up afterwards.

    // create the temp directory
    member val dirName =
        let dname = Path.Combine( Path.GetTempPath(), Guid.NewGuid().ToString() )
        Assert.IsFalse( Directory.Exists( dname ) )
        Directory.CreateDirectory( dname ) |> ignore
        dname

    interface IDisposable with
        member this.Dispose() =
            // clean up
            if Directory.Exists( this.dirName ) then
                Directory.Delete( this.dirName, true )

// --------------------------------------------------------------------

type GitTestRepo ( zipFname ) =
    // Manage a git repo used as input for a test.
    // git doesn't handle nested repo's very well, so we can't check in the repo's we use for testing
    // as they are. Instead, we store them as zip files, unpack them into a temp directory when we want
    // to use them, then clean them up afterwards.

    // set up the repo
    let _tempDir = new TempDir()
    member val repoDir =
        let fname = Path.Combine( __SOURCE_DIRECTORY__, "fixtures", zipFname )
        ZipFile.ExtractToDirectory( fname, _tempDir.dirName )
        _tempDir.dirName

    interface IDisposable with
        member this.Dispose() =
            // clean up
            ( _tempDir :> IDisposable ).Dispose()

// --------------------------------------------------------------------

[<AutoOpen>]
module Utils =

    let checkCapturedOutput (cap: CaptureStdout) fname =
        // compare the captured output with what's expected
        let expected = File.ReadAllText( fname, Encoding.UTF8 )
        let output = cap.getOutput
        if output <> expected then
            let fname2 = Path.Combine( Path.GetTempPath(), "captured-output.txt" )
            File.WriteAllText( fname2, output, Encoding.UTF8 )
            Assert.Fail(
                sprintf "Captured output`mismatch: %s" ( Path.GetFileName( fname ) )
            )
