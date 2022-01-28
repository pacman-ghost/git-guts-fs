namespace tests

open System
open System.Text
open System.IO
open System.IO.Compression
open System.Diagnostics
open Microsoft.VisualStudio.TestTools.UnitTesting

open Spectre.Console

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

type CaptureStdout () =
    // Temporarily capture output sent to stdout.

    // set up a buffer to capture stdout
    let _writer = new StringWriter()
    let _prevOut = System.Console.Out
    let _prevAnsiConsole = AnsiConsole.Console.Profile.Out
    do
        System.Console.SetOut( _writer )
        AnsiConsole.Console.Profile.Out <- new AnsiConsoleOutput( _writer )

    interface IDisposable with
        member this.Dispose() =
            // clean up
            System.Console.SetOut( _prevOut )
            AnsiConsole.Console.Profile.Out <- _prevAnsiConsole
            _writer.Dispose()

    member this.checkOutput fname =
        // compare the captured output with what's expected
        let expected = File.ReadAllText( fname, Encoding.UTF8 )
        let output = this.getOutput
        if output <> expected then
            let fname2 = Path.Combine( Path.GetTempPath(), "captured-output.txt" )
            File.WriteAllText( fname2, output, Encoding.UTF8 )
            Assert.Fail(
                sprintf "Captured output`mismatch: %s" ( Path.GetFileName( fname ) )
            )

    member this.getOutput =
        // return the captured output
        _writer.ToString()

// --------------------------------------------------------------------

[<AutoOpen>]
module Utils =

    let runGit repoDir cmd args =
        // run git and capture the output
        let gitPath = "git" // nb: we assume this is on the PATH
        let gitDir = Path.Combine( repoDir, ".git" )
        let startInfo = ProcessStartInfo( FileName=gitPath, RedirectStandardOutput=true, UseShellExecute=false )
        let addArg arg = startInfo.ArgumentList.Add( arg )
        Seq.iter addArg [| "--git-dir"; gitDir; cmd |]
        Seq.iter addArg args
        let proc = Process.Start( startInfo )
        let output = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        Assert.AreEqual( 0, proc.ExitCode )
        output

    let runGitGc repoDir =
        // run git garbage collection
        runGit repoDir "gc" [] |> ignore
